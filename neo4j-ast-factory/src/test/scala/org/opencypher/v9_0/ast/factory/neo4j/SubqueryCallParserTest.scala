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
package org.opencypher.v9_0.ast.factory.neo4j

import org.opencypher.v9_0.ast.{Clause, Initialization}

class SubqueryCallParserTest extends JavaccParserAstTestBase[Clause] {

  implicit private val parser: JavaccRule[Clause] = JavaccRule.Clause

  test("CALL { RETURN 1 }") {
    gives(subqueryCall(Nil, null, return_(literalInt(1).unaliased)))
  }

  test("CALL { CALL { RETURN 1 as a } }") {
    gives(subqueryCall(Nil, null, subqueryCall(Nil, null, return_(literalInt(1).as("a")))))
  }

  test("CALL { RETURN 1 AS a UNION RETURN 2 AS a }") {
    gives(subqueryCall(unionDistinct(
      singleQuery(return_(literalInt(1).as("a"))),
      singleQuery(return_(literalInt(2).as("a")))
    )))
  }

  test("CALL RECURSIVELY (init=3)(done) { MATCH (a) WHERE id(a) = init RETURN a.y as init, a.foo as done } ") {
    gives(subqueryCall(
      List(Initialization(varFor("init"), literalInt(3))),
      varFor("done"),
      match_(
        nodePat(Some("a")),
        Some(where(equals(function("id", varFor("a")), varFor("init"))))
      ),
      return_(
        prop("a","y").as("init"),
        prop("a","foo").as("done")
      )
    ))
  }

  test("CALL { }") {
    failsToParse
  }

  test("CALL { CREATE (n:N) }") {
    gives(subqueryCall(Nil, null, create(nodePat(Some("n"), Some(labelLeaf("N"))))))
  }
}
