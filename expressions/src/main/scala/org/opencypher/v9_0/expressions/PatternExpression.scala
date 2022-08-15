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

import org.opencypher.v9_0.util.InputPosition

case class PatternExpression(pattern: RelationshipsPattern)(
  override val outerScope: Set[LogicalVariable]
) extends ScopeExpression with ExpressionWithOuterScope with SubqueryExpression {

  override def position: InputPosition = pattern.position

  private val patternVariables = pattern.element.allVariables

  override def introducedVariables: Set[LogicalVariable] = patternVariables -- outerScope

  override def scopeDependencies: Set[LogicalVariable] = patternVariables intersect outerScope

  override def withOuterScope(outerScope: Set[LogicalVariable]): PatternExpression =
    copy()(outerScope)

  override def dup(children: Seq[AnyRef]): this.type = {
    PatternExpression(
      children.head.asInstanceOf[RelationshipsPattern]
    )(outerScope).asInstanceOf[this.type]
  }
}
