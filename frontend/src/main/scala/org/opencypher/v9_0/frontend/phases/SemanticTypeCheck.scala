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
package org.opencypher.v9_0.frontend.phases

import org.opencypher.v9_0.ast.Create
import org.opencypher.v9_0.ast.semantics.SemanticError
import org.opencypher.v9_0.ast.semantics.SemanticTable
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.NamedPatternPart
import org.opencypher.v9_0.expressions.NodePattern
import org.opencypher.v9_0.expressions.Pattern
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.functions.Exists
import org.opencypher.v9_0.frontend.phases.CompilationPhaseTracer.CompilationPhase.SEMANTIC_TYPE_CHECK
import org.opencypher.v9_0.frontend.phases.ListCoercedToBooleanCheck.listCoercedToBooleanCheck
import org.opencypher.v9_0.frontend.phases.PatternExpressionInNonExistenceCheck.patternExpressionInNonExistenceCheck
import org.opencypher.v9_0.frontend.phases.SemanticTypeCheck.SemanticErrorCheck
import org.opencypher.v9_0.util.ErrorMessageProvider
import org.opencypher.v9_0.util.Foldable.FoldableAny
import org.opencypher.v9_0.util.Foldable.SkipChildren
import org.opencypher.v9_0.util.Foldable.TraverseChildren
import org.opencypher.v9_0.util.Ref
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.symbols.CTAny
import org.opencypher.v9_0.util.symbols.CTBoolean
import org.opencypher.v9_0.util.symbols.CTList

/**
 * Checks for semantic errors when semantic table has been initialized.
 *
 * Does not change the State, just checks for semantic errors.
 */
case object SemanticTypeCheck extends Phase[BaseContext, BaseState, BaseState] {

  type SemanticErrorCheck = (BaseState, BaseContext) => Seq[SemanticError]

  val checks: Seq[SemanticErrorCheck] = Seq(
    patternExpressionInNonExistenceCheck,
    CreatePatternSelfReferenceCheck.check,
    listCoercedToBooleanCheck
  )

  override def process(from: BaseState, context: BaseContext): BaseState = {
    context.errorHandler(checks.flatMap(_.apply(from, context)))

    from
  }

  override val phase = SEMANTIC_TYPE_CHECK

  override def postConditions: Set[StepSequencer.Condition] = Set.empty

}

trait ExpectedBooleanTypeCheck {

  def isExpectedTypeBoolean(semanticTable: SemanticTable, e: Expression): Boolean =
    semanticTable.types.get(e)
      .flatMap(_.expected)
      .exists(CTBoolean.covariant.containsAll)
}

object PatternExpressionInNonExistenceCheck extends ExpectedBooleanTypeCheck {

  def patternExpressionInNonExistenceCheck: SemanticErrorCheck = (baseState, _) => {

    baseState.statement().folder.treeFold(Seq.empty[SemanticError]) {
      case Exists(_) =>
        // Don't look inside exists()
        errors => SkipChildren(errors)

      case p: PatternExpression if !isExpectedTypeBoolean(baseState.semanticTable(), p) =>
        errors => SkipChildren(errors :+ SemanticError(errorMessage, p.position))
    }
  }

  val errorMessage: String = "A pattern expression should only be used in order to test the existence of a pattern. " +
    "It should therefore only be used in contexts that evaluate to a boolean, e.g. inside the function exists() or in a WHERE-clause. " +
    "No other uses are allowed, instead they should be replaced by a pattern comprehension."
}

object CreatePatternSelfReferenceCheck {

  def check: SemanticErrorCheck = (baseState, baseContext) => {
    val semanticTable = baseState.semanticTable()
    baseState.statement().folder.treeFold(Seq.empty[SemanticError]) {
      case Create(p) =>
        accErrors =>
          val errors = findSelfReferenceVariablesInPattern(p, semanticTable)
            .map(createError(_, semanticTable, baseContext.errorMessageProvider))
            .toSeq
          SkipChildren(accErrors ++ errors)
    }
  }

  private def findSelfReferenceVariablesInPattern(
    pattern: Pattern,
    semanticTable: SemanticTable
  ): Set[LogicalVariable] = {
    val allSymbolDefinitions = semanticTable.recordedScopes(pattern).allSymbolDefinitions

    def findAllVariables(e: Any): Set[LogicalVariable] = e.folder.findAllByClass[LogicalVariable].toSet
    def isDefinition(variable: LogicalVariable): Boolean =
      allSymbolDefinitions(variable.name).map(_.use).contains(Ref(variable))

    pattern.patternParts.flatMap { patternParts =>
      val (declaredVariables, referencedVariables) =
        patternParts.folder.treeFold[(Set[LogicalVariable], Set[LogicalVariable])]((Set.empty, Set.empty)) {
          case NodePattern(maybeVariable, _, maybeProperties, _) => acc =>
              SkipChildren((acc._1 ++ maybeVariable.filter(isDefinition), acc._2 ++ findAllVariables(maybeProperties)))
          case RelationshipPattern(maybeVariable, _, _, maybeProperties, _, _) => acc =>
              SkipChildren((acc._1 ++ maybeVariable.filter(isDefinition), acc._2 ++ findAllVariables(maybeProperties)))
          case NamedPatternPart(variable, _) => acc => TraverseChildren((acc._1 + variable, acc._2))
        }
      referencedVariables.intersect(declaredVariables)
    }.toSet
  }

  private def createError(
    variable: LogicalVariable,
    semanticTable: SemanticTable,
    errorMessageProvider: ErrorMessageProvider
  ): SemanticError = {
    val msg = errorMessageProvider.createSelfReferenceError(
      variable.name,
      semanticTable.getActualTypeFor(variable).toShortString
    )
    SemanticError(msg, variable.position)
  }
}

object ListCoercedToBooleanCheck extends ExpectedBooleanTypeCheck {

  private def isListCoercedToBoolean(semanticTable: SemanticTable, e: Expression): Boolean = {
    semanticTable.types.get(e).exists(typeInfo =>
      CTList(CTAny).covariant.containsAll(typeInfo.specified) && isExpectedTypeBoolean(semanticTable, e)
    )
  }

  def listCoercedToBooleanCheck: SemanticErrorCheck = (baseState, _) => {

    baseState.statement().folder.treeFold(Seq.empty[SemanticError]) {
      case p: Expression
        if isListCoercedToBoolean(baseState.semanticTable(), p) && !p.isInstanceOf[PatternExpression] =>
        errors => SkipChildren(errors :+ SemanticError(errorMessage, p.position))
    }
  }

  val errorMessage: String = "Coercion of list to boolean is not allowed. Please use `NOT isEmpty(...)` instead."
}
