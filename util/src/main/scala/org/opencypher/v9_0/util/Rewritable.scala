/*
 * Copyright (c) Neo4j Sweden AB (http://neo4j.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.opencypher.v9_0.util

import jdk.jshell.spi.ExecutionControl.InternalException
import org.opencypher.v9_0.util.Foldable.TreeAny
import org.opencypher.v9_0.util.Rewritable.RewritableAny

import java.lang.reflect.Method

import scala.annotation.tailrec
import scala.collection.IterableFactory
import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Rewriter {

  def lift(f: PartialFunction[AnyRef, AnyRef]): Rewriter =
    f.orElse({ case x => x })

  val noop: Rewriter = Rewriter.lift(PartialFunction.empty)
}

object RewriterWithParent {

  def lift(f: PartialFunction[(AnyRef, Option[AnyRef]), AnyRef]): RewriterWithParent =
    f.orElse({ case (x, _) => x })
}

object Rewritable {

  implicit class IteratorEq[A <: AnyRef](val iterator: Iterator[A]) {

    def eqElements[B <: AnyRef](that: Iterator[B]): Boolean = {
      while (iterator.hasNext && that.hasNext) {
        if (!(iterator.next() eq that.next()))
          return false
      }
      !iterator.hasNext && !that.hasNext
    }
  }

  private val productCopyConstructors = new ThreadLocal[mutable.HashMap[Class[_], Method]]() {

    override def initialValue: mutable.HashMap[Class[_], Method] =
      new mutable.HashMap[Class[_], Method]
  }

  def copyConstructor(product: Product): Method = {
    def getCopyMethod(productClass: Class[_ <: Product]): Method = {
      try {
        productClass.getMethods.find(_.getName == "copy").get
      } catch {
        case _: NoSuchElementException =>
          throw new IllegalStateException(
            s"Failed trying to rewrite $productClass - this class does not have a `copy` method"
          )
      }
    }

    val productClass = product.getClass
    productCopyConstructors.get.getOrElseUpdate(productClass, getCopyMethod(productClass))
  }

  def dupAny(that: AnyRef, children: Seq[AnyRef]): AnyRef =
    try {
      if (children.iterator eqElements that.treeChildren) {
        that
      } else {
        that match {
          case a: Rewritable =>
            a.dup(children)
          case _: collection.IndexedSeq[_] =>
            children.toIndexedSeq
          case _: List[_] =>
            children.toList
          case _: collection.Seq[_] =>
            children
          case _: collection.immutable.ListSet[_] =>
            children.to(IterableFactory.toFactory(ListSet))
          case _: collection.Set[_] =>
            children.toSet
          case _: collection.Map[_, _] =>
            children.map(value => value.asInstanceOf[(String, AnyRef)]).toMap
          case p: Product =>
            copyConstructor(p).invoke(p, children: _*)
          case t =>
            t
        }
      }
    } catch {
      case e: IllegalArgumentException =>
        throw new IllegalStateException(s"Failed rewriting $that\nTried using children: ${children.mkString(",")}", e)
    }

  def dupProduct(product: Product, children: Seq[AnyRef]): Product = product match {
    case a: Rewritable =>
      a.dup(children)
    case _ =>
      if (children.iterator eqElements product.treeChildren)
        product
      else
        copyConstructor(product).invoke(product, children: _*).asInstanceOf[Product]
  }

  implicit class RewritableAny[T <: AnyRef](val that: T) extends AnyVal {

    def rewrite(rewriter: Rewriter): AnyRef = {
      val result = rewriter.apply(that)
      result
    }

    def rewrite(rewriter: RewriterWithParent, parent: Option[AnyRef]): AnyRef = {
      val result = rewriter.apply((that, parent))
      result
    }

    def endoRewrite(rewriter: Rewriter): T = rewrite(rewriter).asInstanceOf[T]
  }
}

case class TypedRewriter[T <: Rewritable](rewriter: Rewriter) extends (T => T) {
  def apply(that: T): T = rewriter.apply(that).asInstanceOf[T]

  def narrowed[S <: T]: TypedRewriter[S] = TypedRewriter[S](rewriter)
}

trait Rewritable {
  def dup(children: Seq[AnyRef]): this.type
}

object inSequence {

  private class InSequenceRewriter(rewriters: Seq[Rewriter]) extends Rewriter {

    override def apply(that: AnyRef): AnyRef = {
      val it = rewriters.iterator
      // this piece of code is used a lot and has been through profiling
      // please don't just remove it because it is ugly looking
      var result = that
      while (it.hasNext) {
        result = result.rewrite(it.next())
      }

      result
    }
  }

  private class InSequenceRewriterWithCancel(rewriters: Seq[Rewriter], cancellation: CancellationChecker)
      extends Rewriter {

    override def apply(that: AnyRef): AnyRef = {
      val it = rewriters.iterator
      var result = that
      while (it.hasNext) {
        cancellation.throwIfCancelled()
        result = result.rewrite(it.next())
      }

      result
    }
  }

  def apply(rewriters: Rewriter*): Rewriter =
    new InSequenceRewriter(rewriters)

  def apply(cancellation: CancellationChecker)(rewriters: Rewriter*): Rewriter =
    new InSequenceRewriterWithCancel(rewriters, cancellation)
}

object topDown {

  private class TopDownRewriter(rewriter: Rewriter, stopper: AnyRef => Boolean, cancellation: CancellationChecker)
      extends Rewriter {

    override def apply(that: AnyRef): AnyRef = {
      val initialStack = mutable.Stack((List(that), new mutable.ListBuffer[AnyRef]()))
      val result = rec(initialStack)
      assert(result.size == 1)
      result.head
    }

    @tailrec
    private def rec(stack: mutable.Stack[(List[AnyRef], mutable.ListBuffer[AnyRef])]): mutable.ListBuffer[AnyRef] = {
      cancellation.throwIfCancelled()
      val (currentJobs, _) = stack.top
      if (currentJobs.isEmpty) {
        val (_, newChildren) = stack.pop()
        if (stack.isEmpty) {
          newChildren
        } else {
          stack.pop() match {
            case (job :: jobs, doneJobs) =>
              val doneJob = Rewritable.dupAny(job, newChildren.toSeq)
              stack.push((jobs, doneJobs += doneJob))
              rec(stack)
            case _ => throw new InternalException("Empty job")
          }
        }
      } else {
        stack.pop() match {
          case (newJob :: jobs, doneJobs) =>
            if (stopper(newJob)) {
              stack.push((jobs, doneJobs += newJob))
            } else {
              val rewrittenJob = newJob.rewrite(rewriter)
              stack.push((rewrittenJob :: jobs, doneJobs))
              stack.push((rewrittenJob.treeChildren.toList, new mutable.ListBuffer()))
            }
            rec(stack)
          case _ => throw new InternalException("Empty job")
        }
      }
    }
  }

  def apply(
    rewriter: Rewriter,
    stopper: AnyRef => Boolean = _ => false,
    cancellation: CancellationChecker = CancellationChecker.NeverCancelled
  ): Rewriter =
    new TopDownRewriter(rewriter, stopper, cancellation)
}

/**
 * Top-down rewriter that also lets the rules see the parent of each node as additional context
 */
object topDownWithParent {

  private class TopDownWithParentRewriter(
    rewriter: RewriterWithParent,
    stopper: AnyRef => Boolean,
    cancellation: CancellationChecker
  ) extends Rewriter {

    override def apply(that: AnyRef): AnyRef = {
      val initialStack = mutable.Stack((List(that), new ListBuffer[AnyRef]()))
      val result = rec(initialStack)
      assert(result.size == 1)
      result.head
    }

    @tailrec
    private def rec(stack: mutable.Stack[(List[AnyRef], mutable.ListBuffer[AnyRef])]): mutable.ListBuffer[AnyRef] = {
      cancellation.throwIfCancelled()
      val (currentJobs, _) = stack.top
      if (currentJobs.isEmpty) {
        val (_, newChildren) = stack.pop()
        if (stack.isEmpty) {
          newChildren
        } else {
          stack.pop() match {
            case (job :: jobs, doneJobs) =>
              val doneJob = Rewritable.dupAny(job, newChildren.toSeq)
              stack.push((jobs, doneJobs += doneJob))
              rec(stack)
            case _ => throw new InternalException(s"Empty job")
          }
        }
      } else {
        stack.pop() match {
          case (newJob :: jobs, doneJobs) =>
            if (stopper(newJob)) {
              stack.push((jobs, doneJobs += newJob))
            } else {
              val maybeParent = {
                if (stack.isEmpty) {
                  None
                } else {
                  val (parentJobs, _) = stack.top
                  parentJobs.headOption
                }
              }
              val rewrittenJob = newJob.rewrite(rewriter, maybeParent)
              stack.push((rewrittenJob :: jobs, doneJobs))
              stack.push((rewrittenJob.treeChildren.toList, new ListBuffer()))
            }
            rec(stack)
          case _ => throw new InternalException("Empty jobs")
        }
      }
    }
  }

  def apply(
    rewriter: RewriterWithParent,
    stopper: AnyRef => Boolean = _ => false,
    cancellation: CancellationChecker = CancellationChecker.NeverCancelled
  ): Rewriter =
    new TopDownWithParentRewriter(rewriter, stopper, cancellation)
}

object bottomUp {

  private class BottomUpRewriter(rewriter: Rewriter, stopper: AnyRef => Boolean, cancellation: CancellationChecker)
      extends Rewriter {

    override def apply(that: AnyRef): AnyRef = {
      val initialStack = mutable.Stack((List(that), new ListBuffer[AnyRef]()))
      val result = rec(initialStack)
      assert(result.size == 1)
      result.head
    }

    @tailrec
    private def rec(stack: mutable.Stack[(List[AnyRef], mutable.ListBuffer[AnyRef])]): mutable.ListBuffer[AnyRef] = {
      cancellation.throwIfCancelled()
      val (currentJobs, _) = stack.top
      if (currentJobs.isEmpty) {
        val (_, newChildren) = stack.pop()
        if (stack.isEmpty) {
          newChildren
        } else {
          stack.pop() match {
            case (job :: jobs, doneJobs) =>
              val doneJob = Rewritable.dupAny(job, newChildren.toSeq)
              val rewrittenDoneJob = doneJob.rewrite(rewriter)
              stack.push((jobs, doneJobs += rewrittenDoneJob))
              rec(stack)
            case _ => throw new InternalException("No jobs")
          }
        }
      } else {
        val next = currentJobs.head
        if (stopper(next)) {
          stack.pop() match {
            case (job :: jobs, doneJobs) => stack.push((jobs, doneJobs += job))
            case _                       => throw new InternalException("No jobs")
          }
        } else {
          stack.push((next.treeChildren.toList, new ListBuffer()))
        }
        rec(stack)
      }
    }
  }

  def apply(
    rewriter: Rewriter,
    stopper: AnyRef => Boolean = _ => false,
    cancellation: CancellationChecker = CancellationChecker.NeverCancelled
  ): Rewriter =
    new BottomUpRewriter(rewriter, stopper, cancellation)
}

object bottomUpWithRecorder {

  private class BottomUpRewriter(
    rewriter: Rewriter,
    stopper: AnyRef => Boolean,
    recorder: (AnyRef, AnyRef) => Unit,
    cancellation: CancellationChecker
  ) extends Rewriter {

    override def apply(that: AnyRef): AnyRef = {
      val initialStack = mutable.Stack((List(that), new ListBuffer[AnyRef]()))
      val result = rec(initialStack)
      assert(result.size == 1)
      result.head
    }

    @tailrec
    private def rec(stack: mutable.Stack[(List[AnyRef], mutable.ListBuffer[AnyRef])]): mutable.ListBuffer[AnyRef] = {
      cancellation.throwIfCancelled()
      val (currentJobs, _) = stack.top
      if (currentJobs.isEmpty) {
        val (_, newChildren) = stack.pop()
        if (stack.isEmpty) {
          newChildren
        } else {
          stack.pop() match {
            case (job :: jobs, doneJobs) =>
              val doneJob = Rewritable.dupAny(job, newChildren.toSeq)
              val rewrittenDoneJob = doneJob.rewrite(rewriter)
              if (!(doneJob eq rewrittenDoneJob))
                recorder(doneJob, rewrittenDoneJob)
              stack.push((jobs, doneJobs += rewrittenDoneJob))
              rec(stack)
            case _ => throw new InternalException("Empty jobs")
          }
        }
      } else {
        val next = currentJobs.head
        if (stopper(next)) {
          stack.pop() match {
            case (job :: jobs, doneJobs) => stack.push((jobs, doneJobs += job))
            case _                       => throw new InternalException("Empty jobs")
          }
        } else {
          stack.push((next.treeChildren.toList, new ListBuffer()))
        }
        rec(stack)
      }
    }
  }

  def apply(
    rewriter: Rewriter,
    stopper: AnyRef => Boolean = _ => false,
    recorder: (AnyRef, AnyRef) => Unit = (_, _) => (),
    cancellation: CancellationChecker = CancellationChecker.NeverCancelled
  ): Rewriter =
    new BottomUpRewriter(rewriter, stopper, recorder, cancellation)
}
