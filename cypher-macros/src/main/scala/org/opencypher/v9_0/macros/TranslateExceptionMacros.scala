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
package org.opencypher.v9_0.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object TranslateExceptionMacros {

  /**
   * Wraps an expression in a try-catch block. The catch block rethrows kernel exceptions as cypher exceptions, plus some other mapping rules.
   *
   * @param f the function to wrap in try catch
   * @param tokenNameLookup this should be of type `TokenNameLookup`, which will also be checked by the compiler during macro expansion.
   *                        We avoid declaring this here to avoid circular dependencies.
   */
  def translateException[A](tokenNameLookup: AnyRef, f: A): A = macro translateExceptionImpl[A]

  def translateExceptionImpl[A: c.WeakTypeTag](c: blackbox.Context)(
    tokenNameLookup: c.Tree,
    f: c.Tree
  ): c.universe.Tree = {
    import c.universe.Quasiquote
    q"""
        try {
          $f
        } catch {
          case e: org.neo4j.exceptions.KernelException =>
            throw new org.neo4j.exceptions.CypherExecutionException(e.getUserMessage($tokenNameLookup), e)

          case e: org.neo4j.graphdb.ConstraintViolationException =>
            throw new org.neo4j.exceptions.ConstraintViolationException(e.getMessage, e)

          case e: org.neo4j.kernel.api.exceptions.ResourceCloseFailureException =>
            throw new org.neo4j.exceptions.CypherExecutionException(e.getMessage, e)

          case e: java.lang.ArithmeticException =>
            throw new org.neo4j.exceptions.ArithmeticException(e.getMessage, e)
        }
      """
  }

  def translateIterator[A](tokenNameLookup: AnyRef, iteratorFactory: => Iterator[A]): Iterator[A] =
    macro translateIteratorImp[A]

  def translateIteratorImp[A](c: blackbox.Context)(tokenNameLookup: c.Tree, iteratorFactory: c.Tree)(implicit
  tag: c.WeakTypeTag[A]): c.universe.Tree = {
    import c.universe.Quasiquote
    import c.universe.TypeName
    import c.universe.Ident
    import c.universe.Type
    import c.universe.AppliedTypeTree

    def toTypeTree(typ: Type): c.universe.Tree = {
      val base = Ident(TypeName(typ.typeSymbol.name.toString))
      val args = typ.typeArgs.map(t => toTypeTree(t))

      if (args.isEmpty)
        base
      else
        AppliedTypeTree(base, args)
    }
    val innerTypeTree = toTypeTree(tag.tpe)

    val translatedIterator = translateExceptionImpl(c)(tokenNameLookup, iteratorFactory)
    val translatedNext = translateExceptionImpl(c)(tokenNameLookup, q"innerIterator.next()")
    val translatedHasNext = translateExceptionImpl(c)(tokenNameLookup, q"innerIterator.hasNext")

    q"""
        val innerIterator = $translatedIterator
        new Iterator[$innerTypeTree] {
          override def hasNext: Boolean = $translatedHasNext
          override def next(): $innerTypeTree = $translatedNext
        }
      """
  }
}
