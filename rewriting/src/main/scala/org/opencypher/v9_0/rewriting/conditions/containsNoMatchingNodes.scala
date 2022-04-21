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
package org.opencypher.v9_0.rewriting.conditions

import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.Foldable.FoldableAny
import org.opencypher.v9_0.util.InputPosition

case class containsNoMatchingNodes(matcher: PartialFunction[ASTNode, String]) extends (Any => Seq[String]) {

  def apply(that: Any): Seq[String] = {
    that.folder.fold(Seq.empty[(String, InputPosition)]) {
      case node: ASTNode if matcher.isDefinedAt(node) =>
        acc => acc :+ ((matcher(node), node.position))
    }.map { case (name, position) => s"Expected none but found $name at position $position" }
  }
}
