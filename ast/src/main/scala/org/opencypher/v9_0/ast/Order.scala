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
package org.opencypher.v9_0.ast

import org.opencypher.v9_0.ast.AmbiguousAggregation.ambiguousExpressions
import org.opencypher.v9_0.ast.AmbiguousAggregation.notProjectedAggregationExpression
import org.opencypher.v9_0.ast.Order.ambiguousAggregationMessage
import org.opencypher.v9_0.ast.Order.notProjectedAggregations
import org.opencypher.v9_0.ast.prettifier.ExpressionStringifier
import org.opencypher.v9_0.ast.semantics.SemanticCheck
import org.opencypher.v9_0.ast.semantics.SemanticCheckable
import org.opencypher.v9_0.ast.semantics.SemanticError
import org.opencypher.v9_0.ast.semantics.SemanticExpressionCheck
import org.opencypher.v9_0.ast.semantics.SemanticPatternCheck
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.LogicalProperty
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.Property
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.InputPosition

case class OrderBy(sortItems: Seq[SortItem])(val position: InputPosition) extends ASTNode with SemanticCheckable {
  def semanticCheck: SemanticCheck = sortItems.semanticCheck

  def checkIllegalOrdering(returnItems: ReturnItems): Seq[SemanticError] =
    checkAmbiguousOrdering(returnItems).toSeq ++ checkAggregationInProjection(returnItems)

  private def checkAmbiguousOrdering(returnItems: ReturnItems): Option[SemanticError] = {
    val (aggregationItems, groupingItems) = returnItems.items.partition(item => item.expression.containsAggregate)
    val groupingVariablesAndAliases = groupingItems.map(_.expression).collect { case v: LogicalVariable =>
      v
    } ++ returnItems.items.flatMap(_.alias)
    val propertiesUsedForGrouping =
      groupingItems.map(_.expression).collect { case v @ LogicalProperty(LogicalVariable(_), _) => v }

    if (aggregationItems.nonEmpty) {
      val ambiguousExprs = sortItems.flatMap(sortItem =>
        ambiguousExpressions(
          sortItem.expression,
          groupingVariablesAndAliases,
          propertiesUsedForGrouping
        )
      )

      if (ambiguousExprs.nonEmpty) {
        Some(SemanticError(
          ambiguousAggregationMessage(ambiguousExprs.map(_.asCanonicalStringVal)),
          sortItems.head.position
        ))
      } else {
        None
      }
    } else {
      None
    }
  }

  private def checkAggregationInProjection(returnItems: ReturnItems): Option[SemanticError] = {
    val aggregationItems = returnItems.items
      .filter(item => item.expression.containsAggregate)
      .map(_.expression)

    if (aggregationItems.nonEmpty) {
      val illegalSortItems =
        sortItems.flatMap(sortItem => notProjectedAggregationExpression(sortItem.expression, aggregationItems))

      if (illegalSortItems.nonEmpty) {
        Some(SemanticError(
          notProjectedAggregations(illegalSortItems.map(_.asCanonicalStringVal)),
          sortItems.head.position
        ))
      } else {
        None
      }
    } else {
      None
    }
  }

  def dependencies: Set[LogicalVariable] =
    sortItems.foldLeft(Set.empty[LogicalVariable]) { case (acc, item) => acc ++ item.expression.dependencies }
}

object Order {

  def ambiguousAggregationMessage(variables: Seq[String]): String =
    s"Order by column contains implicit grouping expressions: ${variables.mkString(",")}. Implicit grouping keys are not supported. " +
      "For example, in 'RETURN n.a, n.a + n.b + count(*)' the aggregation expression 'n.a + n.b + count(*)' includes the implicit grouping key 'n.b'. " +
      "It may be possible to rewrite the query by extracting these grouping/aggregation expressions into a preceding WITH clause. "

  def notProjectedAggregations(variables: Seq[String]): String =
    s"Illegal aggregation expression(s) in order by: ${variables.mkString(", ")}. " +
      "If an aggregation expression is used in order by, it also needs to be a projection item on it's own. " +
      "For example, in 'RETURN n.a, 1 + count(*) ORDER BY count(*) + 1' the aggregation expression 'count(*) + 1' is not a projection " +
      "item on its own, but it could be rewritten to 'RETURN n.a, 1 + count(*) AS cnt ORDER BY 1 + count(*)'."
}

sealed trait SortItem extends ASTNode with SemanticCheckable {
  def expression: Expression

  def semanticCheck: SemanticCheck = SemanticExpressionCheck.check(Expression.SemanticContext.Results, expression) chain
    SemanticPatternCheck.checkValidPropertyKeyNames(
      expression.folder.findAllByClass[Property].map(prop => prop.propertyKey)
    )
  def stringify(expressionStringifier: ExpressionStringifier): String
  def mapExpression(f: Expression => Expression): SortItem
}

case class AscSortItem(expression: Expression)(
  val position: InputPosition
) extends SortItem {

  override def mapExpression(f: Expression => Expression): AscSortItem =
    copy(expression = f(expression))(position)

  override def dup(children: Seq[AnyRef]): AscSortItem.this.type =
    AscSortItem(children.head.asInstanceOf[Expression])(position).asInstanceOf[this.type]

  override def asCanonicalStringVal: String = s"${expression.asCanonicalStringVal} ASC"

  override def stringify(expressionStringifier: ExpressionStringifier): String =
    s"${expressionStringifier(expression)} ASC"
}

case class DescSortItem(expression: Expression)(
  val position: InputPosition
) extends SortItem {

  override def mapExpression(f: Expression => Expression): DescSortItem =
    copy(expression = f(expression))(position)

  override def dup(children: Seq[AnyRef]): DescSortItem.this.type =
    DescSortItem(children.head.asInstanceOf[Expression])(position).asInstanceOf[this.type]
  override def asCanonicalStringVal: String = s"${expression.asCanonicalStringVal} DESC"

  override def stringify(expressionStringifier: ExpressionStringifier): String =
    s"${expressionStringifier(expression)} DESC"
}
