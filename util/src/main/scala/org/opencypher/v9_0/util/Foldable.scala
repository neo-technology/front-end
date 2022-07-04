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

import org.opencypher.v9_0.util.Foldable.Folder

import scala.annotation.tailrec
import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.reflect.ClassTag

object Foldable {

  implicit class TreeAny(val that: Any) extends AnyVal {

    def treeChildren: Iterator[AnyRef] = that match {
      case s: collection.Seq[_] => s.iterator.asInstanceOf[Iterator[AnyRef]]
      case s: Set[_]            => s.iterator.asInstanceOf[Iterator[AnyRef]]
      case m: Map[_, _]         => m.iterator.asInstanceOf[Iterator[AnyRef]]
      case p: Product           => p.productIterator.asInstanceOf[Iterator[AnyRef]]
      case _                    => Iterator.empty.asInstanceOf[Iterator[AnyRef]]
    }

    def reverseTreeChildren: Iterator[AnyRef] = that match {
      case s: collection.Seq[_] => s.reverseIterator.asInstanceOf[Iterator[AnyRef]]
      // For list sets, the order matters. However, they are otherwise matched as Sets and therefore not reversed.
      // Therefore, we need to handle them separately.
      case s: ListSet[_] => reverseListSetIterator(s).asInstanceOf[Iterator[AnyRef]]
      case s: Set[_]     => s.iterator.asInstanceOf[Iterator[AnyRef]]
      case m: Map[_, _]  => m.iterator.asInstanceOf[Iterator[AnyRef]]
      case p: Product    => reverseProductIterator(p)
      case _             => Iterator.empty.asInstanceOf[Iterator[AnyRef]]
    }

    def reverseListSetIterator[A](listSet: ListSet[A]): Iterator[A] = {
      var values = listSet
      val reversed = List.newBuilder[A]
      while (values.nonEmpty) {
        reversed.addOne(values.last)
        values = values.init
      }
      reversed.result().iterator
    }

    private def reverseProductIterator(p: Product) = new Iterator[AnyRef] {
      private var c: Int = p.productArity - 1
      override def hasNext: Boolean = c >= 0
      override def next(): AnyRef = { val result = p.productElement(c).asInstanceOf[AnyRef]; c -= 1; result }
    }
  }

  /**
   * Define the folding behavior on a node during a tree fold.
   */
  sealed trait FoldingBehavior[R]

  /**
   * Do not traverse into children. Use the given accumulator to continue into the siblings.
   */
  case class SkipChildren[R](acc: R) extends FoldingBehavior[R]

  /**
   * Traverse into children. Use the given accumulator to continue into the children.
   */
  case class TraverseChildren[R](acc: R) extends FoldingBehavior[R]

  /**
   * Traverse into children. Use `accumulatorForChildren` to continue into the children.
   * After having traversed the children, use `forSiblings` to transform the
   * output accumulator for this "inner treefold" to create the accumulator
   * for traversing the siblings of the node.
   */
  case class TraverseChildrenNewAccForSiblings[R](accumulatorForChildren: R, forSiblings: R => R)
      extends FoldingBehavior[R]

  /**
   * Any type is foldable
   */
  implicit class FoldableAny(override val foldedOver: Any) extends Foldable

  /**
   * Exposes the various fold methods
   */
  class Folder(foldedOver: Any, cancellation: CancellationChecker) {

    def fold[R](init: R)(f: PartialFunction[Any, R => R]): R =
      foldAcc(mutable.Stack(foldedOver), init, f.lift, cancellation)

    /**
     * Fold of a tree structure
     *
     * This treefold will traverse the tree structure with a BFS strategy which can be customized as follows.
     *
     * The treefold behaviour is controlled by the given partial function: when the partial function is undefined on a
     * given node of the tree, that node is simply ignored and the tree traversal will continue.  If the partial
     * function is defined on the node than it will visited in different ways, depending on the output of the function
     * returned, as explained below.
     *
     * This function will be called with the current accumulator and it is expected to produce a FoldingBehavior.
     * That will contain the next accumulator and specifies if and how to traverse children of the current node.
     *
     * @param init the initial value of the accumulator
     * @param f    partial function that given a node in the tree might return a function that takes the current
     *             accumulator, and returns a FoldingBehavior that specifies if and how to traverse children of the current node.
     * @tparam R the type of the accumulator/result
     * @return the accumulated result
     */
    def treeFold[R](init: R)(f: PartialFunction[Any, R => FoldingBehavior[R]]): R = {
      treeFoldAcc(
        mutable.Stack(foldedOver),
        init,
        f.andThen[R => (R, Option[R => R])](innerF => innerTreeFold(innerF)).lift,
        new mutable.Stack[(mutable.Stack[Any], R => R)](),
        reverse = false,
        cancellation
      )
    }

    /**
     * Same as [[treeFold]], but traverses the nodes in reverse order.
     */
    def reverseTreeFold[R](init: R)(f: PartialFunction[Any, R => FoldingBehavior[R]]): R =
      treeFoldAcc(
        mutable.Stack(foldedOver),
        init,
        f.andThen[R => (R, Option[R => R])](innerF => innerTreeFold(innerF)).lift,
        new mutable.Stack[(mutable.Stack[Any], R => R)](),
        reverse = true,
        cancellation
      )

    private def innerTreeFold[R](innerF: R => FoldingBehavior[R]): R => (R, Option[R => R]) = acc => {
      innerF(acc) match {
        case SkipChildren(newAcc)                                         => (newAcc, None)
        case TraverseChildren(newAcc)                                     => (newAcc, Some(identity))
        case TraverseChildrenNewAccForSiblings(newAcc, mergeAccumulators) => (newAcc, Some(mergeAccumulators))
      }
    }

    /*
    Allows searching through object tree and object collections
     */
    def treeExists(f: PartialFunction[Any, Boolean]): Boolean =
      existsAcc(mutable.Stack(foldedOver), f.lift, cancellation)

    /*
    Allows searching through object tree and object collections
     */
    def treeFind[A: ClassTag](f: PartialFunction[A, Boolean]): Option[A] =
      findAcc[A](mutable.Stack(foldedOver), f.lift, cancellation)

    /*
    Allows searching through object tree and object collections for a class
     */
    def treeFindByClass[A: ClassTag]: Option[A] =
      optionFindAcc[A](mutable.ArrayStack(foldedOver), cancellation)

    /*
    Searches in trees, counting how many matches are found
     */
    def treeCount(f: PartialFunction[Any, Boolean]): Int = {
      val lifted = f.lift
      countAcc(mutable.Stack(foldedOver), (a: Any) => lifted(a).map(_ => 1), 0, cancellation)
    }

    def treeCountAccumulation(f: PartialFunction[Any, Int]): Int = {
      countAcc(mutable.Stack(foldedOver), f.lift, 0, cancellation)
    }

    def findAllByClass[A: ClassTag]: Seq[A] = {
      val remaining = mutable.Stack(foldedOver)
      val result = mutable.ListBuffer[A]()

      while (remaining.nonEmpty) {
        cancellation.throwIfCancelled()
        val that = remaining.pop()
        that match {
          case x: A => result += x
          case _    =>
        }
        remaining.pushAll(that.reverseTreeChildren)
      }

      result
    }.toSeq
  }

  @tailrec
  private def foldAcc[R](
    remaining: mutable.Stack[Any],
    acc: R,
    f: Any => Option[R => R],
    cancellation: CancellationChecker
  ): R = {
    cancellation.throwIfCancelled()
    if (remaining.isEmpty) {
      acc
    } else {
      val that = remaining.pop()
      foldAcc(remaining.pushAll(that.reverseTreeChildren), f(that).fold(acc)(_(acc)), f, cancellation)
    }
  }

  @tailrec
  private def treeFoldAcc[R](
    remaining: mutable.Stack[Any],
    acc: R,
    f: Any => Option[R => (R, Option[R => R])],
    continuation: mutable.Stack[(mutable.Stack[Any], R => R)],
    reverse: Boolean,
    cancellation: CancellationChecker
  ): R = {
    cancellation.throwIfCancelled()
    if (remaining.isEmpty) {
      if (continuation.isEmpty) {
        acc
      } else {
        val (stack, contAccFunc) = continuation.pop()
        treeFoldAcc(stack, contAccFunc(acc), f, continuation, reverse, cancellation)
      }
    } else {
      val that = remaining.pop()
      f(that) match {
        case None =>
          val children = if (reverse) that.treeChildren else that.reverseTreeChildren
          treeFoldAcc(remaining.pushAll(children), acc, f, continuation, reverse, cancellation)
        case Some(pf) =>
          pf(acc) match {
            case (newAcc, Some(contAccFunc)) =>
              continuation.push((remaining, contAccFunc))
              val children = if (reverse) that.treeChildren else that.reverseTreeChildren
              treeFoldAcc(mutable.Stack[Any]().pushAll(children), newAcc, f, continuation, reverse, cancellation)
            case (newAcc, None) =>
              treeFoldAcc(remaining, newAcc, f, continuation, reverse, cancellation)
          }
      }
    }
  }

  @tailrec
  private def existsAcc(
    remaining: mutable.Stack[Any],
    f: Any => Option[Boolean],
    cancellation: CancellationChecker
  ): Boolean = {
    cancellation.throwIfCancelled()
    if (remaining.isEmpty) {
      false
    } else {
      val that = remaining.pop()
      f(that) match {
        case Some(true) =>
          true
        case _ =>
          existsAcc(remaining.pushAll(that.reverseTreeChildren), f, cancellation)
      }
    }
  }

  @tailrec
  private def countAcc(
    remaining: mutable.Stack[Any],
    f: Any => Option[Int],
    acc: Int,
    cancellation: CancellationChecker
  ): Int = {
    cancellation.throwIfCancelled()
    if (remaining.isEmpty) {
      acc
    } else {
      val that = remaining.pop()
      val next = f(that) match {
        case Some(value) =>
          acc + value
        case _ =>
          acc
      }

      countAcc(remaining.pushAll(that.reverseTreeChildren), f, next, cancellation)
    }
  }

  @tailrec
  private def findAcc[A: ClassTag](
    remaining: mutable.Stack[Any],
    cancellation: CancellationChecker
  ): A = {
    cancellation.throwIfCancelled()
    if (remaining.isEmpty) {
      throw new NoSuchElementException
    } else {
      val that = remaining.pop()
      that match {
        case x: A => x
        case _    => findAcc(remaining.pushAll(that.reverseTreeChildren), cancellation)
      }
    }
  }

  @tailrec
  private def optionFindAcc[A: ClassTag](
    remaining: mutable.ArrayStack[Any],
    cancellation: CancellationChecker
  ): Option[A] = {
    cancellation.throwIfCancelled()
    if (remaining.isEmpty) {
      None
    } else {
      val that = remaining.pop()
      that match {
        case x: A => Some(x)
        case _    => optionFindAcc(remaining ++= that.reverseTreeChildren, cancellation)
      }
    }
  }

  @tailrec
  private def findAcc[A: ClassTag](
    remaining: mutable.Stack[Any],
    predicate: A => Option[Boolean],
    cancellation: CancellationChecker
  ): Option[A] = {
    cancellation.throwIfCancelled()
    if (remaining.isEmpty) {
      None
    } else {
      val that = remaining.pop()
      that match {
        case x: A if predicate(x).isDefined => Some(x)
        case _ => findAcc(remaining.pushAll(that.reverseTreeChildren), predicate, cancellation)
      }
    }
  }
}

trait Foldable {
  def foldedOver: Any = this

  def folder: Folder =
    new Folder(foldedOver, CancellationChecker.NeverCancelled)

  def folder(cancellation: CancellationChecker): Folder =
    new Folder(foldedOver, cancellation)
}
