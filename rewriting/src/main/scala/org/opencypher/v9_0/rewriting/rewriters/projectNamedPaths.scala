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

import org.opencypher.v9_0.ast
import org.opencypher.v9_0.ast.AliasedReturnItem
import org.opencypher.v9_0.ast.ProjectionClause
import org.opencypher.v9_0.ast.ReturnItem
import org.opencypher.v9_0.ast.ReturnItems
import org.opencypher.v9_0.ast.SingleQuery
import org.opencypher.v9_0.ast.SubqueryCall
import org.opencypher.v9_0.ast.With
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions
import org.opencypher.v9_0.expressions.AnonymousPatternPart
import org.opencypher.v9_0.expressions.EveryPath
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.MultiRelationshipPathStep
import org.opencypher.v9_0.expressions.NamedPatternPart
import org.opencypher.v9_0.expressions.NilPathStep
import org.opencypher.v9_0.expressions.NodePathStep
import org.opencypher.v9_0.expressions.NodePattern
import org.opencypher.v9_0.expressions.PathExpression
import org.opencypher.v9_0.expressions.PathStep
import org.opencypher.v9_0.expressions.PatternElement
import org.opencypher.v9_0.expressions.RelationshipChain
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.ShortestPaths
import org.opencypher.v9_0.expressions.SingleRelationshipPathStep
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.rewriting.conditions.SemanticInfoAvailable
import org.opencypher.v9_0.rewriting.conditions.containsNamedPathOnlyForShortestPath
import org.opencypher.v9_0.rewriting.conditions.containsNoReturnAll
import org.opencypher.v9_0.rewriting.rewriters.factories.ASTRewriterFactory
import org.opencypher.v9_0.util.AnonymousVariableNameGenerator
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.Foldable.FoldableAny
import org.opencypher.v9_0.util.Foldable.SkipChildren
import org.opencypher.v9_0.util.Foldable.TraverseChildren
import org.opencypher.v9_0.util.Foldable.TraverseChildrenNewAccForSiblings
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.Ref
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.symbols.CypherType
import org.opencypher.v9_0.util.topDown

import scala.annotation.tailrec

case object projectNamedPaths extends Rewriter with StepSequencer.Step with ASTRewriterFactory {

  case class Projectibles(
    paths: Map[Variable, PathExpression] = Map.empty,
    protectedVariables: Set[Ref[LogicalVariable]] = Set.empty,
    variableRewrites: Map[Ref[LogicalVariable], PathExpression] = Map.empty,
    insertedWiths: Map[SingleQuery, With] = Map.empty
  ) {

    self =>

    def withoutNamedPaths: Projectibles = copy(paths = Map.empty)

    def withProtectedVariable(ident: Ref[LogicalVariable]): Projectibles =
      copy(protectedVariables = protectedVariables + ident)
    def withNamedPath(entry: (Variable, PathExpression)): Projectibles = copy(paths = paths + entry)

    def withRewrittenVariable(entry: (Ref[LogicalVariable], PathExpression)): Projectibles = {
      val (ref, pathExpr) = entry
      copy(variableRewrites = variableRewrites + (ref -> pathExpr.endoRewrite(copyVariables)))
    }

    def withInsertedWith(query: SingleQuery, wizz: With): Projectibles =
      copy(insertedWiths = insertedWiths + (query -> wizz))

    def withVariableRewritesForExpression(expr: Expression): Projectibles =
      expr.folder.treeFold(self) {
        case ident: Variable =>
          acc =>
            acc.paths.get(ident) match {
              case Some(pathExpr) => TraverseChildren(acc.withRewrittenVariable(Ref(ident) -> pathExpr))
              case None           => TraverseChildren(acc)
            }
      }
  }

  object Projectibles {
    val empty: Projectibles = Projectibles()
  }

  def apply(input: AnyRef): AnyRef = instance(input)

  private val instance: Rewriter = input => {
    val Projectibles(_, protectedVariables, variableRewrites, insertedWiths) = collectProjectibles(input)
    val applicator = Rewriter.lift {

      case ident: Variable if !protectedVariables(Ref(ident)) =>
        variableRewrites.getOrElse(Ref(ident), ident)

      case namedPart @ NamedPatternPart(_, _: ShortestPaths) =>
        namedPart

      case NamedPatternPart(_, part) =>
        part

      case expr: PathExpression =>
        expr

      case singleQuery: SingleQuery if insertedWiths.contains(singleQuery) =>
        val newImportingWith = insertedWiths(singleQuery)
        singleQuery.copy(clauses = newImportingWith +: singleQuery.clauses)(singleQuery.position)
    }
    topDown(applicator)(input)
  }

  private def collectProjectibles(input: AnyRef): Projectibles = input.folder.treeFold(Projectibles.empty) {
    case aliased: AliasedReturnItem =>
      acc =>
        // We are not allowed to replace the alias of a ReturnItem, so we add it to the protected variables.
        TraverseChildren(acc.withProtectedVariable(Ref(aliased.variable)))

    case ident: Variable =>
      acc =>
        // Collect rewritten variables that refer to path variables
        acc.paths.get(ident) match {
          case Some(pathExpr) => TraverseChildren(acc.withRewrittenVariable(Ref(ident) -> pathExpr))
          case None           => TraverseChildren(acc)
        }

    case projection: ProjectionClause =>
      acc =>
        // Collect rewritten variables inside of the ReturnItems that refer to path variables
        val projectedAcc = projection.returnItems.items.map(_.expression).foldLeft(acc) {
          (acc, expr) => acc.withVariableRewritesForExpression(expr)
        }
        // After this projection, we remove all named paths. They have either been projected here, or they are not available in the rest of the query.
        TraverseChildrenNewAccForSiblings(projectedAcc, _.withoutNamedPaths)

    case subquery: SubqueryCall =>
      acc =>
        // Collect importing WITH clauses to insert into subqueries.
        // Importing with clauses cannot contain PathExpressions, so we need to add an extra WITH clause before those with all the variables from the path.
        val newAcc = subquery.part.folder.treeFold(acc) {
          case query: SingleQuery => innerAcc =>
              val allReturnItems: Seq[ReturnItem] = query.importWith.collect {
                case With(_, ReturnItems(_, items, _), _, _, _, _, _) => items
              }.getOrElse(Seq[ReturnItem]())

              val (pathReturnItems, nonPathReturnItems) = allReturnItems.partition {
                // We can assume all return items are aliased at this point
                case AliasedReturnItem(v: Variable, _) if acc.paths.keySet.contains(v) => true
                case _                                                                 => false
              }

              val returnItemsWithVariablesFromPaths: Seq[AliasedReturnItem] =
                acc.paths.collect {
                  case (variable, pathExpression) if pathReturnItems.exists(_.expression == variable) =>
                    pathExpression.step.dependencies
                }.flatten.map(v => ast.AliasedReturnItem(v, v)(InputPosition.NONE, isAutoAliased = true)).toSeq

              val newImportingWith: Option[With] = {
                if (returnItemsWithVariablesFromPaths.isEmpty) {
                  None
                } else {
                  val returnItems: Seq[ReturnItem] = (returnItemsWithVariablesFromPaths ++ nonPathReturnItems).distinct
                  Some(With(
                    distinct = false,
                    ReturnItems(includeExisting = false, returnItems)(InputPosition.NONE),
                    None,
                    None,
                    None,
                    None
                  )(InputPosition.NONE))
                }
              }
              val newAcc = newImportingWith.map(w => innerAcc.withInsertedWith(query, w)).getOrElse(innerAcc)
              SkipChildren(newAcc)
        }
        TraverseChildren(newAcc)

    case _: SingleQuery =>
      acc =>
        // When coming out of a query (either inside a UNION or inside a Subquery), we need to restore the paths that were available before.
        // WITH clauses inside of the query might have removed the paths.
        TraverseChildrenNewAccForSiblings(acc, insideAcc => insideAcc.copy(paths = acc.paths))

    case NamedPatternPart(_, _: ShortestPaths) =>
      acc =>
        // We do not want to replace named shortest paths
        TraverseChildren(acc)

    case part @ NamedPatternPart(variable, patternPart) =>
      acc =>
        // Remember the named path for replacing variables referring to it
        val pathExpr = expressions.PathExpression(patternPartPathExpression(patternPart))(part.position)
        TraverseChildren(acc.withNamedPath(variable -> pathExpr).withProtectedVariable(Ref(variable)))
  }

  def patternPartPathExpression(patternPart: AnonymousPatternPart): PathStep = patternPart match {
    case EveryPath(element) => patternPartPathExpression(element)
    case x                  => throw new IllegalStateException(s"Unknown pattern part: $x")
  }

  def patternPartPathExpression(element: PatternElement): PathStep = flip(element, NilPathStep()(element.position))

  @tailrec
  private def flip(element: PatternElement, step: PathStep): PathStep = {
    element match {
      case np @ NodePattern(node, _, _, _) =>
        NodePathStep(node.get.copyId, step)(np.position)

      case rc @ RelationshipChain(leftSide, RelationshipPattern(rel, _, length, _, _, direction), to) =>
        length match {
          case None =>
            flip(
              leftSide,
              SingleRelationshipPathStep(rel.get.copyId, direction, to.variable.map(_.copyId), step)(rc.position)
            )

          case Some(_) =>
            flip(
              leftSide,
              MultiRelationshipPathStep(rel.get.copyId, direction, to.variable.map(_.copyId), step)(rc.position)
            )
        }

      case _ =>
        throw new IllegalArgumentException("Quantified path patterns in a named path are not allowed at the moment.")
    }
  }

  override def preConditions: Set[StepSequencer.Condition] = Set(
    // This rewriter needs to know the expanded return items
    containsNoReturnAll,
    NoNamedPathsInPatternComprehensions
  )

  override def postConditions: Set[StepSequencer.Condition] = Set(
    containsNamedPathOnlyForShortestPath
  )

  override def invalidatedConditions: Set[StepSequencer.Condition] = SemanticInfoAvailable // Introduces new AST nodes

  override def getRewriter(
    semanticState: SemanticState,
    parameterTypeMapping: Map[String, CypherType],
    cypherExceptionFactory: CypherExceptionFactory,
    anonymousVariableNameGenerator: AnonymousVariableNameGenerator
  ): Rewriter = instance
}
