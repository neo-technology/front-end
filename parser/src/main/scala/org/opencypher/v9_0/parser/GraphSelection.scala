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
package org.opencypher.v9_0.parser

import org.opencypher.v9_0.ast
import org.parboiled.scala.Parser
import org.parboiled.scala.Rule1
import org.parboiled.scala.group

trait GraphSelection extends Parser
                     with Expressions
                     with Base {
  def FromGraph: Rule1[ast.FromGraph] = rule("FROM GRAPH") {
    group(keyword("FROM") ~~ optional(keyword("GRAPH"))) ~~ Expression ~~>> (ast.FromGraph(_))
  }

  def UseGraph: Rule1[ast.UseGraph] = rule("USE GRAPH") {
    group(keyword("USE") ~~ optional(keyword("GRAPH"))) ~~ Expression ~~>> (ast.UseGraph(_))
  }
}