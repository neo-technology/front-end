/*
 * Copyright © 2002-2020 Neo4j Sweden AB (http://neo4j.com)
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

import org.opencypher.v9_0.ast.ASTAnnotationMap
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.LabelName
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.PropertyKeyName
import org.opencypher.v9_0.expressions.RelTypeName
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.LabelId
import org.opencypher.v9_0.util.PropertyKeyId
import org.opencypher.v9_0.util.RelTypeId
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.symbols.CTAny
import org.opencypher.v9_0.util.symbols.CTList
import org.opencypher.v9_0.util.symbols.CTNode
import org.opencypher.v9_0.util.symbols.CTRelationship
import org.opencypher.v9_0.util.symbols.TypeSpec

import scala.collection.mutable

object SemanticTable {
  def apply(types: ASTAnnotationMap[Expression, ExpressionTypeInfo] = ASTAnnotationMap.empty,
            recordedScopes: ASTAnnotationMap[ASTNode, Scope] = ASTAnnotationMap.empty) =
    new SemanticTable(types, recordedScopes)
}

class SemanticTable(
                     val types: ASTAnnotationMap[Expression, ExpressionTypeInfo] = ASTAnnotationMap.empty,
                     val recordedScopes: ASTAnnotationMap[ASTNode, Scope] = ASTAnnotationMap.empty,
                     val resolvedLabelNames: mutable.Map[String, LabelId] = new mutable.HashMap[String, LabelId],
                     val resolvedPropertyKeyNames: mutable.Map[String, PropertyKeyId] = new mutable.HashMap[String, PropertyKeyId],
                     val resolvedRelTypeNames: mutable.Map[String, RelTypeId] = new mutable.HashMap[String, RelTypeId]
  ) extends Cloneable {

  def getTypeFor(s: String): TypeSpec = try {
    val reducedType = types.collect {
      case (Variable(name), typ) if name == s => typ.specified
    }.reduce(_ & _)

    if (reducedType.isEmpty)
      throw new IllegalStateException(s"This semantic table contains conflicting type information for variable $s")

    reducedType
  } catch {
    case e: UnsupportedOperationException =>
      throw new IllegalStateException(s"Did not find any type information for variable $s", e)
  }

  def getActualTypeFor(expr: Expression): TypeSpec =
    types.getOrElse(expr, throw new IllegalStateException(s"Did not find any type information for expression $expr")).actual

  def containsNode(expr: String): Boolean = types.exists {
    case (v@Variable(name), _) => name == expr && isNode(v) // NOTE: Profiling showed that checking node type last is better
    case _ => false
  }

  def id(labelName:LabelName):Option[LabelId] = resolvedLabelNames.get(labelName.name)

  def id(propertyKeyName:PropertyKeyName):Option[PropertyKeyId] = resolvedPropertyKeyNames.get(propertyKeyName.name)

  def id(resolvedRelTypeName:RelTypeName):Option[RelTypeId] = resolvedRelTypeNames.get(resolvedRelTypeName.name)

  def seen(expression: Expression): Boolean = types.contains(expression)

  def isNode(expr: String): Boolean = getTypeFor(expr) == CTNode.invariant

  def isRelationship(expr: String): Boolean = getTypeFor(expr) == CTRelationship.invariant

  def isRelationshipCollection(expr: String): Boolean = getTypeFor(expr) == CTList(CTRelationship).invariant

  def isNodeCollection(expr: String): Boolean = getTypeFor(expr) == CTList(CTNode).invariant

  def isNode(expr: Expression): Boolean = types(expr).specified == CTNode.invariant

  /**
   * Same as isNode, but will simply return false if no semantic information is available instead of failing.
   */
  def isNodeNoFail(expr: Expression): Boolean = types.get(expr).map(_.specified).contains(CTNode.invariant)

  def isRelationship(expr: Expression): Boolean = types(expr).specified == CTRelationship.invariant

  /**
   * Same as isRelationship, but will simply return false if no semantic information is available instead of failing.
   */
  def isRelationshipNoFail(expr: Expression): Boolean = types.get(expr).map(_.specified).contains(CTRelationship.invariant)

  def addNode(expr: Variable): SemanticTable =
    copy(types = types.updated(expr, ExpressionTypeInfo(CTNode.invariant, None)))

  def addRelationship(expr: Variable): SemanticTable =
    copy(types = types.updated(expr, ExpressionTypeInfo(CTRelationship.invariant, None)))

  def addTypeInfoCTAny(expr: Expression): SemanticTable =
    copy(types = types.updated(expr, ExpressionTypeInfo(CTAny.invariant, None)))

  def replaceExpressions(rewriter: Rewriter): SemanticTable = {
    val replacements = types.keys.toIndexedSeq.map { keyExpression =>
      keyExpression -> keyExpression.endoRewrite(rewriter)
    }
    copy(types = types.replaceKeys(replacements: _*), recordedScopes = recordedScopes.replaceKeys(replacements: _*))
  }

  def replaceNodes(replacements: (ASTNode, ASTNode)*): SemanticTable =
    copy(recordedScopes = recordedScopes.replaceKeys(replacements: _*))

  def symbolDefinition(variable: Variable): SymbolUse =
    recordedScopes(variable).symbolTable(variable.name).definition

  override def clone(): SemanticTable = copy()

  def copy(
            types: ASTAnnotationMap[Expression, ExpressionTypeInfo] = types,
            recordedScopes: ASTAnnotationMap[ASTNode, Scope] = recordedScopes,
            resolvedLabelIds: mutable.Map[String, LabelId] = resolvedLabelNames,
            resolvedPropertyKeyNames: mutable.Map[String, PropertyKeyId] = resolvedPropertyKeyNames,
            resolvedRelTypeNames: mutable.Map[String, RelTypeId] = resolvedRelTypeNames
  ) =
    new SemanticTable(types, recordedScopes, resolvedLabelIds.clone(), resolvedPropertyKeyNames.clone(), resolvedRelTypeNames.clone())
}
