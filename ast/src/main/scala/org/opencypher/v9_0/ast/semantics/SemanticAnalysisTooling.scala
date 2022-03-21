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
package org.opencypher.v9_0.ast.semantics

import org.opencypher.v9_0.expressions.DoubleLiteral
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.Expression.DefaultTypeMismatchMessageGenerator
import org.opencypher.v9_0.expressions.Expression.SemanticContext
import org.opencypher.v9_0.expressions.IntegerLiteral
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.Parameter
import org.opencypher.v9_0.expressions.TypeSignature
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.CTAny
import org.opencypher.v9_0.util.symbols.CypherType
import org.opencypher.v9_0.util.symbols.TypeSpec

/**
 * This class holds methods for performing semantic analysis.
 */
trait SemanticAnalysisTooling {

  def semanticCheckFold[A](
                     traversable: Iterable[A]
                   )(
                     f:A => SemanticCheck
  ): SemanticCheck = {
    traversable.foldLeft(SemanticCheck.success) {
      (prevCheck, o: A) => prevCheck chain f(o)
    }
  }

  def semanticCheck[A <: SemanticCheckable](traversable: IterableOnce[A]): SemanticCheck =
    (state: SemanticState) =>
      traversable.foldLeft(SemanticCheckResult.success(state)) {
        (r1: SemanticCheckResult, o: A) =>
          {
            val r2 = o.semanticCheck(r1.state)
            SemanticCheckResult(r2.state, r1.errors ++ r2.errors)
          }
      }

  /** Runs `check` on `state`. Discards produced state, but retains produced errors */
  def withState(state: SemanticState)(check: SemanticCheck): SemanticCheck = (s: SemanticState) =>
    check(state).copy(state = s)

  def specifyType(
    typeGen: TypeGenerator,
    expression: Expression
  ): SemanticState => Either[SemanticError, SemanticState] =
    (s: SemanticState) => specifyType(typeGen(s), expression)(s)

  def specifyType(
    possibleTypes: => TypeSpec,
    expression: Expression
  ): SemanticState => Either[SemanticError, SemanticState] =
    _.specifyType(expression, possibleTypes)

  def expectType(typeGen: TypeGenerator, expression: Expression): SemanticCheck =
    (s: SemanticState) => expectType(typeGen(s), expression)(s)

  def expectType(possibleTypes: TypeSpec, opt: Option[Expression]): SemanticCheck =
    opt.foldSemanticCheck(expectType(possibleTypes, _))

  def expectType(
    typeGen: TypeGenerator,
    expression: Expression,
    messageGen: (String, String) => String
  ): SemanticCheck =
    (s: SemanticState) => expectType(typeGen(s), expression, messageGen)(s)

  def expectType[Exp <: Expression](possibleTypes: TypeSpec, expressions: Iterable[Exp]): SemanticCheck =
    (state: SemanticState) =>
      expressions.foldLeft(SemanticCheckResult.success(state)) {
        (r1: SemanticCheckResult, o: Exp) =>
          {
            val r2 = expectType(possibleTypes, o)(r1.state)
            SemanticCheckResult(r2.state, r1.errors ++ r2.errors)
          }
      }

  def expectType(
    possibleTypes: => TypeSpec
  )(
    ctx: SemanticContext,
    expr: Expression
  ): SemanticCheck = expectType(possibleTypes, expr)

  def expectType(
    possibleTypes: => TypeSpec,
    expression: Expression,
    messageGen: (String, String) => String = DefaultTypeMismatchMessageGenerator
  ): SemanticCheck = (s: SemanticState) => {
    s.expectType(expression, possibleTypes) match {
      case (ss, TypeSpec.none) =>
        val existingTypesString = ss.expressionType(expression).specified.mkString(", ", " or ")
        val expectedTypesString = possibleTypes.mkString(", ", " or ")
        expression match {
          case p: Parameter
            if !p.name.matches(
              """\s\sAUTO(INT|STRING|DOUBLE|LIST)\d+"""
            ) => // See literalReplacement for list of all AUTOs
            SemanticCheckResult.error(
              ss,
              SemanticError(
                "Type mismatch for parameter '" + p.name + "': " + messageGen(expectedTypesString, existingTypesString),
                expression.position
              )
            )
          case _ =>
            SemanticCheckResult.error(
              ss,
              SemanticError(
                "Type mismatch: " + messageGen(expectedTypesString, existingTypesString),
                expression.position
              )
            )
        }
      case (ss, _) =>
        SemanticCheckResult.success(ss)
    }
  }

  def checkTypes(expression: Expression, signatures: Seq[TypeSignature]): SemanticCheck = (s: SemanticState) => {
    val initSignatures = signatures.filter(_.argumentTypes.length == expression.arguments.length)

    val (remainingSignatures: Seq[TypeSignature], result) =
      expression.arguments.foldLeft((initSignatures, SemanticCheckResult.success(s))) {
        case (accumulator @ (Seq(), _), _) =>
          accumulator
        case ((possibilities, r1), arg) =>
          val argTypes = possibilities.foldLeft(TypeSpec.none) { _ | _.argumentTypes.head.covariant }
          val r2 = expectType(argTypes, arg)(r1.state)

          val actualTypes = types(arg)(r2.state)
          val remainingPossibilities = possibilities.filter {
            sig => actualTypes containsAny sig.argumentTypes.head.covariant
          } map {
            sig => sig.removeFirstArgumentType
          }
          (remainingPossibilities, SemanticCheckResult(r2.state, r1.errors ++ r2.errors))
      }

    val outputType = remainingSignatures match {
      case Seq() => TypeSpec.all
      case _     => remainingSignatures.foldLeft(TypeSpec.none) { _ | _.outputType.invariant }
    }

    specifyType(outputType, expression)(result.state) match {
      case Left(err)    => SemanticCheckResult(result.state, result.errors :+ err)
      case Right(state) => SemanticCheckResult(state, result.errors)
    }
  }

  def whenState(condition: SemanticState => Boolean)(
    thenBranch: => SemanticCheck,
    elseBranch: => SemanticCheck = SemanticCheck.success
  ): SemanticCheck = (state: SemanticState) =>
    if (condition(state))
      thenBranch(state)
    else
      elseBranch(state)

  def unless(condition: Boolean)(check: => SemanticCheck): SemanticCheck =
    if (condition)
      SemanticCheck.success
    else
      check

  def unionOfTypes(traversable: IterableOnce[Expression]): TypeGenerator = (state: SemanticState) =>
    TypeSpec.union(traversable.map(types(_)(state)).toSeq: _*)

  def leastUpperBoundsOfTypes(traversable: IterableOnce[Expression]): TypeGenerator =
    if (traversable.isEmpty)
      _ => CTAny.invariant
    else
      (state: SemanticState) => traversable.map { types(_)(state) } reduce { _ leastUpperBounds _ }

  def withScopedState(check: => SemanticCheck): SemanticCheck =
    SemanticAnalysisTooling.pushStateScope chain
      check chain
      SemanticAnalysisTooling.popStateScope

  def typeSwitch(expr: Expression)(choice: TypeSpec => SemanticCheck): SemanticCheck =
    (state: SemanticState) => choice(state.expressionType(expr).actual)(state)

  def validNumber(long: IntegerLiteral): Boolean =
    try {
      long.value.isInstanceOf[Long]
    } catch {
      case e: java.lang.NumberFormatException => false
    }

  def validNumber(double: DoubleLiteral): Boolean =
    try {
      double.value.isInstanceOf[Double]
    } catch {
      case e: java.lang.NumberFormatException => false
    }

  def ensureDefined(v: LogicalVariable): SemanticState => Either[SemanticError, SemanticState] =
    (_: SemanticState).ensureVariableDefined(v)

  def declareVariable(
    v: LogicalVariable,
    possibleTypes: TypeSpec
  ): SemanticState => Either[SemanticError, SemanticState] =
    (_: SemanticState).declareVariable(v, possibleTypes)

  /**
   * @param overriding if `true` then a previous occurrence of that variable is overridden.
   *                   if `false` then a previous occurrence of that variable leads to an error
   */
  def declareVariable(
    v: LogicalVariable,
    typeGen: TypeGenerator,
    maybePreviousDeclaration: Option[Symbol] = None,
    overriding: Boolean = false
  ): SemanticState => Either[SemanticError, SemanticState] =
    (s: SemanticState) =>
      s.declareVariable(v, typeGen(s), maybePreviousDeclaration, overriding)

  def implicitVariable(
    v: LogicalVariable,
    possibleType: CypherType
  ): SemanticState => Either[SemanticError, SemanticState] =
    (_: SemanticState).implicitVariable(v, possibleType)

  def declareVariables(symbols: Iterable[Symbol]): SemanticCheck =
    symbols.foldSemanticCheck(symbol => declareVariable(symbol.definition.asVariable, symbol.types))

  def recordCurrentScope(astNode: ASTNode): SemanticCheck = { state: SemanticState =>
    SemanticCheckResult.success(state.recordCurrentScope(astNode))
  }

  def requireFeatureSupport(msg: String, feature: SemanticFeature, position: InputPosition): SemanticCheck =
    (s: SemanticState) => {
      if (!s.features(feature))
        SemanticCheckResult.error(
          s,
          FeatureError(
            s"$msg is not available in this implementation of Cypher " +
              s"due to lack of support for $feature.",
            feature,
            position
          )
        )
      else
        SemanticCheckResult.success(s)
    }

  def error(msg: String, position: InputPosition): SemanticCheck = SemanticCheck.error(SemanticError(msg, position))

  def possibleTypes(expression: Expression): TypeGenerator =
    types(expression)(_).unwrapLists

  def types(expression: Expression): TypeGenerator = _.expressionType(expression).actual
}

object SemanticAnalysisTooling {
  private val pushStateScope: SemanticCheck = (state: SemanticState) => SemanticCheckResult.success(state.newChildScope)
  private val popStateScope: SemanticCheck = (state: SemanticState) => SemanticCheckResult.success(state.popScope)
}
