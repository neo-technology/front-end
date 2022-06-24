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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.expressions.And
import org.opencypher.v9_0.expressions.BinaryOperatorExpression
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.False
import org.opencypher.v9_0.expressions.In
import org.opencypher.v9_0.expressions.ListLiteral
import org.opencypher.v9_0.expressions.Not
import org.opencypher.v9_0.expressions.Or
import org.opencypher.v9_0.expressions.ScopeExpression
import org.opencypher.v9_0.rewriting.rewriters.factories.PreparatoryRewritingRewriterFactory
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.Foldable.SkipChildren
import org.opencypher.v9_0.util.InternalNotificationLogger
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.StepSequencer.Condition
import org.opencypher.v9_0.util.StepSequencer.Step
import org.opencypher.v9_0.util.bottomUp

case object LiteralsAreAvailable extends Condition
case object MultipleInPredicatesAreMerged extends Condition

/**
 * Merges multiple IN predicates into one.
 *
 * Examples:
 * MATCH (n) WHERE n.prop IN [1,2,3] AND n.prop IN [2,3,4] RETURN n.prop
 * -> MATCH (n) WHERE n.prop IN [2,3]
 *
 * MATCH (n) WHERE n.prop IN [1,2,3] OR n.prop IN [2,3,4] RETURN n.prop
 * -> MATCH (n) WHERE n.prop IN [1,2,3,4]
 *
 * MATCH (n) WHERE n.prop IN [1,2,3] AND n.prop IN [4,5,6] RETURN n.prop
 * -> MATCH (n) WHERE FALSE
 *
 * NOTE: this rewriter must be applied before auto parameterization, since after
 * that we are just dealing with opaque parameters.
 */
case object mergeInPredicates extends Step with PreparatoryRewritingRewriterFactory {

  override def preConditions: Set[StepSequencer.Condition] = Set(LiteralsAreAvailable)

  override def postConditions: Set[StepSequencer.Condition] = Set(MultipleInPredicatesAreMerged)

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set.empty

  val instance: Rewriter = bottomUp(Rewriter.lift {

    case and @ And(lhs, rhs) if containNeitherOrsNorInnerScopes(lhs, rhs) && containIns(lhs, rhs) =>
      if (containNoNots(lhs, rhs))
        // Look for a `IN [...] AND a IN [...]` and compute the intersection of lists
        rewriteBinaryOperator(and, (a, b) => a intersect b, (l, r) => and.copy(l, r)(and.position))
      else if (containNots(lhs, rhs))
        // Look for a `NOT IN [...] AND a NOT IN [...]` and compute the union of lists
        rewriteBinaryOperator(and, (a, b) => a concat b, (l, r) => and.copy(l, r)(and.position))
      else
        // In case only one of lhs and rhs includes a NOT we cannot rewrite
        and

    case or @ Or(lhs, rhs) if containNoAnds(lhs, rhs) && containIns(lhs, rhs) =>
      if (containNoNots(lhs, rhs))
        // Look for `a IN [...] OR a IN [...]` and compute union of lists
        rewriteBinaryOperator(or, (a, b) => a concat b, (l, r) => or.copy(l, r)(or.position))
      else if (containNots(lhs, rhs))
        // Look for a `NOT IN [...] OR a NOT IN [...]` and compute the intersection of lists
        rewriteBinaryOperator(or, (a, b) => a intersect b, (l, r) => or.copy(l, r)(or.position))
      else
        // In case only one of lhs and rhs includes a NOT we cannot rewrite
        or
  })

  private def containNeitherOrsNorInnerScopes(expressions: Expression*): Boolean =
    expressions.forall(!_.folder.treeExists {
      case _: Or              => true
      case _: ScopeExpression => true
    })

  private def containNoAnds(expressions: Expression*): Boolean = expressions.forall(!_.folder.treeExists {
    case _: And => true
  })

  private def containNots(expressions: Expression*): Boolean = expressions.forall(_.folder.treeExists {
    case _: Not => true
  })

  private def containNoNots(expressions: Expression*): Boolean = expressions.forall(!_.folder.treeExists {
    case _: Not => true
  })

  private def containIns(expressions: Expression*): Boolean = expressions.forall(_.folder.treeExists {
    case _: In => true
  })

  // Takes a binary operator a merge operator and a copy constructor
  // and rewrites the binary operator
  private def rewriteBinaryOperator(
    binary: BinaryOperatorExpression,
    merge: (Seq[Expression], Seq[Expression]) => Seq[Expression],
    copy: (Expression, Expression) => Expression
  ): Expression = {
    val rewriter = inRewriter(collectInPredicates(merge)(binary.lhs, binary.rhs))
    val newLhs = binary.lhs.endoRewrite(rewriter)
    val newRhs = binary.rhs.endoRewrite(rewriter)
    if (newLhs == newRhs)
      newLhs
    else
      copy(newLhs, newRhs)
  }

  // Rewrites a IN [] by using the the provided map of precomputed lists
  // a IN ... is rewritten to a IN inPredicates(a)
  private def inRewriter(inPredicates: Map[Expression, Seq[Expression]]) = bottomUp(Rewriter.lift({
    case in @ In(a, list @ ListLiteral(_)) =>
      val expressions = inPredicates(a)
      if (expressions.nonEmpty)
        in.copy(rhs = list.copy(expressions)(list.position))(in.position)
      else
        False()(in.position)
  }))

  // Given `a IN A ... b IN B ... a IN C` and use `merge` to merge all the lists with the same key.
  // Returns {a -> merge(A,B), b -> C}
  private def collectInPredicates(merge: (Seq[Expression], Seq[Expression]) => Seq[Expression])(
    expressions: Expression*
  ): Map[Expression, Seq[Expression]] = {
    val maps = expressions.map(_.folder.treeFold(Map.empty[Expression, Seq[Expression]]) {
      case In(a, ListLiteral(exprs)) => map => {
          // if there is already a list associated with `a`, do map(a) ++ exprs otherwise exprs
          val values = map.get(a).map(current => merge(current, exprs)).getOrElse(exprs).distinct
          SkipChildren(map + (a -> values))
        }
    })
    // Take list of maps, [map1,map2,...] and merge the using the provided `merge` to
    // merge lists
    maps.reduceLeft((acc, current) => {
      val sharedKeys = acc.keySet intersect current.keySet
      val updates = sharedKeys.map(k => k -> merge(acc(k), current(k)).distinct)
      acc ++ updates ++ (current -- sharedKeys)
    })
  }

  override def getRewriter(
    cypherExceptionFactory: CypherExceptionFactory,
    notificationLogger: InternalNotificationLogger
  ): Rewriter = instance
}
