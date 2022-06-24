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

import org.opencypher.v9_0.ast.AddedInRewrite
import org.opencypher.v9_0.ast.Clause
import org.opencypher.v9_0.ast.CommandClause
import org.opencypher.v9_0.ast.ProjectionClause
import org.opencypher.v9_0.ast.Return
import org.opencypher.v9_0.ast.ReturnItems
import org.opencypher.v9_0.ast.SingleQuery
import org.opencypher.v9_0.ast.TransactionsCommandClause
import org.opencypher.v9_0.ast.Where
import org.opencypher.v9_0.ast.With
import org.opencypher.v9_0.ast.Yield
import org.opencypher.v9_0.rewriting.conditions.containsNoReturnAll
import org.opencypher.v9_0.rewriting.rewriters.factories.PreparatoryRewritingRewriterFactory
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.Foldable.TraverseChildren
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.InternalNotificationLogger
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.StepSequencer.Condition
import org.opencypher.v9_0.util.StepSequencer.Step
import org.opencypher.v9_0.util.bottomUp

import scala.annotation.tailrec

case object ShowRewrittenToShowYieldWhereReturn extends Condition

// We would want this to be part of a later phase, e.g. ASTRewriter.
// That is problematic because it needs to run expandStar, but the YIELD and RETURN clauses need scoping information.
// A solution would be to introduce another pass of SemanticAnalysis between this rewriter and expandStar.
// However, we really don't need more passes of SemanticAnalysis.
// It is not possible to modify or create scope at this point, to do so we would need to be a phase.
// Because of the restricted nature of SHOW commands being only `SHOW ... YIELD ... RETURN ...` this works for now,
// but we should revisit if we become more complicated/flexible.
case object rewriteShowQuery extends Step with PreparatoryRewritingRewriterFactory {

  override def preConditions: Set[StepSequencer.Condition] = Set.empty

  override def postConditions: Set[StepSequencer.Condition] = Set(ShowRewrittenToShowYieldWhereReturn)

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set(
    // Introduces YIELD * and RETURN *
    containsNoReturnAll,
    // It can invalidate this condition by introducing YIELD and RETURN clauses
    ProjectionClausesHaveSemanticInfo
  )

  val instance: Rewriter = bottomUp(Rewriter.lift {
    case s @ SingleQuery(clauses) => s.copy(clauses = rewriteClauses(clauses.toList, List()))(s.position)
  })

  @tailrec
  private def rewriteClauses(clauses: List[Clause], rewrittenClause: List[Clause]): List[Clause] = clauses match {
    // Just a single transaction command clause (with or without WHERE)
    case (commandClause: TransactionsCommandClause) :: Nil =>
      rewrittenClause ++ rewriteToWithAndReturn(commandClause, commandClause.where)
    // Just a single command clause (with or without WHERE)
    case (commandClause: CommandClause) :: Nil =>
      rewrittenClause ++ rewriteWithYieldAndReturn(commandClause, commandClause.where)
    // Transaction command clause with only a YIELD/WITH
    case (commandClause: TransactionsCommandClause) :: (withClause: With) :: Nil =>
      rewrittenClause :+ commandClause :+ updateDefaultOrderOnYield(withClause, commandClause) :+ returnClause(
        lastPosition(withClause),
        getDefaultOrderFromYieldOrCommand(withClause, commandClause)
      )
    // Command clause with only a YIELD
    case (commandClause: CommandClause) :: (yieldClause: Yield) :: Nil =>
      rewrittenClause :+ commandClause :+ updateDefaultOrderOnYield(yieldClause, commandClause) :+ returnClause(
        lastPosition(yieldClause),
        getDefaultOrderFromYieldOrCommand(yieldClause, commandClause)
      )
    // Transaction command clause with YIELD/WITH and RETURN * (to fix column order)
    case (commandClause: TransactionsCommandClause) :: (withClause: With) :: (returnClause: Return) :: Nil
      if returnClause.returnItems.includeExisting =>
      rewrittenClause :+ commandClause :+ updateDefaultOrderOnYield(
        withClause,
        commandClause
      ) :+ updateDefaultOrderOnReturn(
        returnClause,
        withClause,
        commandClause
      )
    // Command clause with YIELD and RETURN * (to fix column order)
    case (commandClause: CommandClause) :: (yieldClause: Yield) :: (returnClause: Return) :: Nil
      if returnClause.returnItems.includeExisting =>
      rewrittenClause :+ commandClause :+ updateDefaultOrderOnYield(
        yieldClause,
        commandClause
      ) :+ updateDefaultOrderOnReturn(
        returnClause,
        yieldClause,
        commandClause
      )
    case c :: cs => rewriteClauses(cs, rewrittenClause :+ c)
    case Nil     => rewrittenClause
  }

  private def getDefaultOrderFromYieldOrCommand(projClause: ProjectionClause, commandClause: CommandClause) =
    if (projClause.returnItems.includeExisting)
      projClause.returnItems.defaultOrderOnColumns.getOrElse(commandClause.unfilteredColumns.columns.map(_.name))
    else projClause.returnItems.items.map(_.name).toList

  private def updateDefaultOrderOnReturn(
    returnClause: Return,
    projClause: ProjectionClause,
    commandClause: CommandClause
  ) = {
    val defaultOrderOnColumns =
      returnClause.returnItems.defaultOrderOnColumns.getOrElse(getDefaultOrderFromYieldOrCommand(
        projClause,
        commandClause
      ))
    returnClause.withReturnItems(returnClause.returnItems.withDefaultOrderOnColumns(defaultOrderOnColumns))
  }

  private def updateDefaultOrderOnYield(projClause: ProjectionClause, commandClause: CommandClause) = {
    val defaultOrderOnColumns = getDefaultOrderFromYieldOrCommand(projClause, commandClause)
    projClause.copyProjection(returnItems = projClause.returnItems.withDefaultOrderOnColumns(defaultOrderOnColumns))
  }

  private def rewriteWithYieldAndReturn(commandClause: CommandClause, where: Option[Where]): List[Clause] = {
    val defaultColumnOrder = commandClause.unfilteredColumns.columns.map(_.name)
    List(
      commandClause.moveWhereToYield,
      Yield(
        ReturnItems(includeExisting = true, Seq(), Some(defaultColumnOrder))(commandClause.position),
        None,
        None,
        None,
        where
      )(commandClause.position),
      returnClause(commandClause.position, defaultColumnOrder)
    )
  }

  private def rewriteToWithAndReturn(commandClause: CommandClause, where: Option[Where]): List[Clause] = {
    val defaultColumnOrder = commandClause.unfilteredColumns.columns.map(_.name)
    List(
      commandClause.moveWhereToYield,
      With(
        distinct = false,
        ReturnItems(includeExisting = true, Seq(), Some(defaultColumnOrder))(commandClause.position),
        None,
        None,
        None,
        where,
        withType = AddedInRewrite
      )(commandClause.position),
      returnClause(commandClause.position, defaultColumnOrder)
    )
  }

  private def returnClause(position: InputPosition, defaultOrderOnColumns: List[String]): Return =
    Return(ReturnItems(includeExisting = true, Seq(), Some(defaultOrderOnColumns))(position))(position)
      .copy(addedInRewrite = true)(position)

  private def lastPosition(c: ProjectionClause): InputPosition = {
    c.folder.treeFold(InputPosition.NONE) {
      case node: ASTNode => acc => TraverseChildren(Seq(acc, node.position).max)
    }
  }

  override def getRewriter(
    cypherExceptionFactory: CypherExceptionFactory,
    notificationLogger: InternalNotificationLogger
  ): Rewriter = instance
}
