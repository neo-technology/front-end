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
package org.opencypher.v9_0.expressions

import org.opencypher.v9_0.expressions.Expression.TreeAcc
import org.opencypher.v9_0.expressions.functions.Rand
import org.opencypher.v9_0.expressions.functions.RandomUUID
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.Foldable.SkipChildren
import org.opencypher.v9_0.util.Foldable.TraverseChildren
import org.opencypher.v9_0.util.Foldable.TraverseChildrenNewAccForSiblings
import org.opencypher.v9_0.util.Ref
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.bottomUp

object Expression {
  sealed trait SemanticContext

  object SemanticContext {
    case object Simple extends SemanticContext
    case object Results extends SemanticContext
  }

  val DefaultTypeMismatchMessageGenerator =
    (expected: String, existing: String) => s"expected $expected but was $existing"

  /**
   * An Accumulator for walking over an Expressions tree while keeping track of variables in scope.
   *
   * @param data the accumulated data
   * @param list a list of scopes, where each element is a Set of Variables in that scope.
   */
  final case class TreeAcc[A](data: A, list: List[Set[LogicalVariable]] = List.empty) {
    def mapData(f: A => A): TreeAcc[A] = copy(data = f(data))

    def inScope(variable: LogicalVariable): Boolean = list.exists(_.contains(variable))
    def variablesInScope: Set[LogicalVariable] = list.toSet.flatten

    def pushScope(newVariable: LogicalVariable): TreeAcc[A] = pushScope(Set(newVariable))
    def pushScope(newVariables: Set[LogicalVariable]): TreeAcc[A] = copy(list = newVariables :: list)
    def popScope: TreeAcc[A] = copy(list = list.tail)
  }

  def mapExpressionHasPropertyReadDependency(mapEntityName: String, mapExpression: Expression): Boolean =
    mapExpression match {
      case MapExpression(items) => items.exists {
          case (k, v) => v.subExpressions.exists {
              case LogicalProperty(LogicalVariable(entityName), propertyKey) =>
                entityName == mapEntityName && propertyKey == k
              case _ => false
            }
        }
      case _ => false
    }

  def hasPropertyReadDependency(entityName: String, expression: Expression, propertyKey: PropertyKeyName): Boolean =
    expression.subExpressions.exists {
      case LogicalProperty(LogicalVariable(name), key) =>
        name == entityName && key == propertyKey
      case _ =>
        false
    }
}

abstract class Expression extends ASTNode {

  self =>

  /**
   * Collects the immediate arguments to this Expression.
   */
  def arguments: Seq[Expression] = this.folder.treeFold(List.empty[Expression]) {
    case e: Expression if e != this =>
      acc => SkipChildren(acc :+ e)
  }

  /**
   * Collects all sub-expressions recursively .
   */
  def subExpressions: Seq[Expression] = this.folder.treeFold(List.empty[Expression]) {
    case e: Expression if e != this =>
      acc => TraverseChildren(acc :+ e)
  }

  /** 
   * All variables referenced from this expression or any of its children
   * that are not introduced inside this expression.
   */
  def dependencies: Set[LogicalVariable] =
    this.folder.treeFold(Expression.TreeAcc[Set[LogicalVariable]](Set.empty)) {
      case scope: ScopeExpression =>
        acc =>
          val newDependencies = scope.dependencies.filterNot(acc.inScope)
          val newAcc = acc.mapData(_ ++ newDependencies)
          SkipChildren(newAcc)
      case id: LogicalVariable => acc => {
          val newAcc = if (acc.inScope(id)) acc else acc.mapData(_ + id)
          TraverseChildren(newAcc)
        }
    }.data

  /** 
   * All (free) occurrences of variable in this expression or any of its children
   * (i.e. excluding occurrences referring to shadowing redefinitions of variable).
   */
  private def occurrences(variable: LogicalVariable): Set[Ref[Variable]] =
    this.folder.treeFold(Expression.TreeAcc[Set[Ref[Variable]]](Set.empty)) {
      case scope: ScopeExpression =>
        acc =>
          val newAcc = acc.pushScope(scope.introducedVariables)
          TraverseChildrenNewAccForSiblings(newAcc, _.popScope)
      case occurrence: Variable if occurrence.name == variable.name =>
        acc => {
          val newAcc = if (acc.inScope(occurrence)) acc else acc.mapData(_ + Ref(occurrence))
          TraverseChildren(newAcc)
        }
    }.data

  /**
   * Replaces all occurrences of a variable in this Expression by the given replacements.
   * This takes into account scoping and does not replace other Variables with the same name
   * in nested inner scopes.
   *
   * @param variable    the variable to replace
   * @param replacement the replacement expression
   * @return this expression with `variable` replaced by `replacement`.
   */
  def replaceAllOccurrencesBy(variable: LogicalVariable, replacement: Expression): Expression = {
    val occurrencesToReplace = occurrences(variable)
    self.endoRewrite(bottomUp(Rewriter.lift {
      case occurrence: Variable if occurrencesToReplace(Ref(occurrence)) => replacement
    }))
  }

  /** 
   * List of child expressions together with any of its dependencies introduced
   * by any of its parent expressions (where this expression is the root of the tree).
   */
  def inputs: Seq[(Expression, Set[LogicalVariable])] =
    this.folder.treeFold(TreeAcc[Seq[(Expression, Set[LogicalVariable])]](Seq.empty)) {
      case scope: ScopeExpression =>
        acc =>
          val newAcc = acc.pushScope(scope.introducedVariables)
            .mapData(pairs => pairs :+ (scope -> acc.variablesInScope))
          TraverseChildrenNewAccForSiblings(newAcc, _.popScope)

      case expr: Expression =>
        acc =>
          val newAcc = acc.mapData(pairs => pairs :+ (expr -> acc.variablesInScope))
          TraverseChildren(newAcc)
    }.data

  /**
   * Return true is this expression contains an aggregating expression.
   */
  def containsAggregate: Boolean = this.folder.treeExists {
    case IsAggregate(_) => true
  }

  /**
   * Returns the first encountered aggregate expression, or None if none existed.
   */
  def findAggregate: Option[Expression] = this.folder.treeFind[Expression] {
    case IsAggregate(_) => true
  }

  def isDeterministic: Boolean = !this.folder.treeExists {
    case f: FunctionInvocation if f.function == Rand || f.function == RandomUUID => true
    case _                                                                       => false
  }
}

/**
 * Signifies that this expression doesn't have to be coerced if used as a predicate
 */
trait BooleanExpression extends Expression
