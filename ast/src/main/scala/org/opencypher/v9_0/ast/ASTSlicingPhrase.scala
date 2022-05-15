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

import org.opencypher.v9_0.ast.ASTSlicingPhrase.checkExpressionIsStaticInt
import org.opencypher.v9_0.ast.semantics.SemanticAnalysisTooling
import org.opencypher.v9_0.ast.semantics.SemanticCheck
import org.opencypher.v9_0.ast.semantics.SemanticCheck.when
import org.opencypher.v9_0.ast.semantics.SemanticCheckable
import org.opencypher.v9_0.ast.semantics.SemanticExpressionCheck
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.Literal
import org.opencypher.v9_0.expressions.PathExpression
import org.opencypher.v9_0.expressions.PatternComprehension
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.SignedDecimalIntegerLiteral
import org.opencypher.v9_0.expressions.UnsignedDecimalIntegerLiteral
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.symbols.CTInteger

// Skip/Limit
trait ASTSlicingPhrase extends SemanticCheckable with SemanticAnalysisTooling {
  self: ASTNode =>
  def name: String
  def expression: Expression
  def semanticCheck: SemanticCheck = checkExpressionIsStaticInt(expression, name, acceptsZero = true)
}

object ASTSlicingPhrase extends SemanticAnalysisTooling {

  /**
   * Checks that the given expression
   *
   *  - contains no variable references
   *  - does not try to read the graph
   *  - is a CTInteger
   *  - is either non-negative or positive, depending on `acceptsZero`
   *
   * @param expression  the expression to check
   * @param name        the name of the construct. Used for error messages.
   * @param acceptsZero if `true` then 0 is an accepted value, otherwise not.
   * @return a SemanticCheck
   */
  def checkExpressionIsStaticInt(expression: Expression, name: String, acceptsZero: Boolean): SemanticCheck =
    containsNoVariables(expression, name) chain
      doesNotTouchTheGraph(expression, name) chain
      literalShouldBeUnsignedInteger(expression, name, acceptsZero) chain
      SemanticExpressionCheck.simple(expression) chain
      expectType(CTInteger.covariant, expression)

  private def containsNoVariables(expression: Expression, name: String): SemanticCheck = {
    val deps = expression.dependencies
    if (deps.nonEmpty) {
      val id = deps.toSeq.minBy(_.position)
      error(s"It is not allowed to refer to variables in $name", id.position)
    } else SemanticCheck.success
  }

  private def doesNotTouchTheGraph(expression: Expression, name: String): SemanticCheck = {
    val badExpressionFound = expression.folder.treeExists {
      case _: PatternComprehension |
        _: PatternExpression |
        _: PathExpression =>
        true
    }
    when(badExpressionFound) {
      error(s"It is not allowed to refer to variables in $name", expression.position)
    }
  }

  private def literalShouldBeUnsignedInteger(
    expression: Expression,
    name: String,
    acceptsZero: Boolean
  ): SemanticCheck = {
    try {
      expression match {
        case _: UnsignedDecimalIntegerLiteral                              => SemanticCheck.success
        case i: SignedDecimalIntegerLiteral if i.value > 0                 => SemanticCheck.success
        case i: SignedDecimalIntegerLiteral if i.value == 0 && acceptsZero => SemanticCheck.success
        case lit: Literal =>
          val accepted = if (acceptsZero) "non-negative" else "positive"
          error(
            s"Invalid input. '${lit.asCanonicalStringVal}' is not a valid value. Must be a $accepted integer.",
            lit.position
          )
        case _ => SemanticCheck.success
      }
    } catch {
      case _: NumberFormatException =>
        // We rely on getting a SemanticError from SemanticExpressionCheck.simple(expression)
        SemanticCheck.success
    }
  }
}
