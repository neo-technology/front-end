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
package org.opencypher.v9_0.rewriting

import org.opencypher.v9_0.ast
import org.opencypher.v9_0.ast.semantics.SemanticTable
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.FunctionInvocation
import org.opencypher.v9_0.expressions.FunctionName
import org.opencypher.v9_0.expressions.LabelExpression.ColonDisjunction
import org.opencypher.v9_0.expressions.Namespace
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.Property
import org.opencypher.v9_0.expressions.PropertyKeyName
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.AnonymousVariableNameGenerator
import org.opencypher.v9_0.util.DeprecatedCoercionOfListToBoolean
import org.opencypher.v9_0.util.DeprecatedRelTypeSeparatorNotification
import org.opencypher.v9_0.util.InternalNotification
import org.opencypher.v9_0.util.Ref
import org.opencypher.v9_0.util.symbols.CTAny
import org.opencypher.v9_0.util.symbols.CTBoolean
import org.opencypher.v9_0.util.symbols.CTList

object Deprecations {

  def propertyOf(propertyKey: String): Expression => Expression =
    e => Property(e, PropertyKeyName(propertyKey)(e.position))(e.position)

  def renameFunctionTo(newName: String): FunctionInvocation => FunctionInvocation =
    f => f.copy(functionName = FunctionName(newName)(f.functionName.position))(f.position)

  def renameFunctionTo(newNamespace: Namespace, newName: String): FunctionInvocation => FunctionInvocation =
    f => f.copy(namespace = newNamespace, functionName = FunctionName(newName)(f.functionName.position))(f.position)

  case object syntacticallyDeprecatedFeaturesIn4_X extends SyntacticDeprecations {

    override val find: PartialFunction[Any, Deprecation] = {

      // timestamp
      case f @ FunctionInvocation(namespace, FunctionName(name), _, _)
        if namespace.parts.isEmpty && name.equalsIgnoreCase("timestamp") =>
        Deprecation(
          Some(Ref(f) -> renameFunctionTo("datetime").andThen(propertyOf("epochMillis"))(f)),
          None
        )

      // legacy type separator -[:A|:B]->
      case rel @ RelationshipPattern(variable, Some(labelExpression), None, None, None, _)
        // this restriction is necessary because in all other cases, this is an error
        if variable.forall(variable => !AnonymousVariableNameGenerator.isNamed(variable.name)) &&
          !labelExpression.containsGpmSpecificRelTypeExpression &&
          labelExpression.folder.findAllByClass[ColonDisjunction].nonEmpty =>
        Deprecation(
          Some(Ref(rel) -> rel.copy(labelExpression = Some(labelExpression.replaceColonSyntax))(rel.position)),
          Some(DeprecatedRelTypeSeparatorNotification(
            labelExpression.folder.findAllByClass[ColonDisjunction].head.position
          ))
        )
    }

  }

  case object semanticallyDeprecatedFeaturesIn4_X extends SemanticDeprecations {

    private def isExpectedTypeBoolean(semanticTable: SemanticTable, e: Expression) =
      semanticTable.types.get(e).exists(typeInfo => typeInfo.expected.fold(false)(CTBoolean.covariant.containsAll))

    private def isListCoercedToBoolean(semanticTable: SemanticTable, e: Expression): Boolean =
      semanticTable.types.get(e).exists(typeInfo =>
        CTList(CTAny).covariant.containsAll(typeInfo.specified) && isExpectedTypeBoolean(semanticTable, e)
      )

    override def find(semanticTable: SemanticTable): PartialFunction[Any, Deprecation] = {
      case e: Expression if isListCoercedToBoolean(semanticTable, e) && !e.isInstanceOf[PatternExpression] =>
        Deprecation(
          None,
          Some(DeprecatedCoercionOfListToBoolean(e.position))
        )
    }
  }
}

/**
 * One deprecation.
 *
 * This class holds both the ability to replace a part of the AST with the preferred non-deprecated variant, and
 * the ability to generate an optional notification to the user that they are using a deprecated feature.
 *
 * @param replacement  an optional replacement tuple with the ASTNode to be replaced and its replacement.
 * @param notification optional appropriate deprecation notification
 */
case class Deprecation(replacement: Option[(Ref[ASTNode], ASTNode)], notification: Option[InternalNotification])

sealed trait Deprecations

trait SyntacticDeprecations extends Deprecations {
  def find: PartialFunction[Any, Deprecation]
  def findWithContext(statement: ast.Statement): Set[Deprecation] = Set.empty
}

trait SemanticDeprecations extends Deprecations {
  def find(semanticTable: SemanticTable): PartialFunction[Any, Deprecation]
}
