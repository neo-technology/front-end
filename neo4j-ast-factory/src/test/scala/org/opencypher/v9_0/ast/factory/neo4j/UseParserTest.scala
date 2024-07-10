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

import org.opencypher.v9_0.ast.Statement

class UseParserTest extends JavaccParserAstTestBase[Statement] {

  implicit private val parser: JavaccRule[Statement] = JavaccRule.Statement

  test("USING PERIODIC COMMIT USE db LOAD CSV FROM 'url' AS line RETURN line") {
    failsToParse
  }

  test("USE GRAPH db USING PERIODIC COMMIT LOAD CSV FROM 'url' AS line RETURN line") {
    failsToParse
  }

  test("CALL { USE neo4j RETURN 1 AS y } RETURN y") {
    gives {
      query(
        subqueryCall(
          Nil,
          null,
          use(varFor("neo4j")),
          returnLit(1 -> "y")
        ),
        return_(variableReturnItem("y"))
      )
    }
  }

  test("WITH 1 AS x CALL { WITH x USE neo4j RETURN x AS y } RETURN x, y") {
    gives {
      query(
        with_(literal(1) as "x"),
        subqueryCall(
          Nil,
          null,
          with_(variableReturnItem("x")),
          use(varFor("neo4j")),
          return_(varFor("x") as "y")
        ),
        return_(variableReturnItem("x"), variableReturnItem("y"))
      )
    }
  }

  test("USE foo UNION ALL RETURN 1") {
    gives {
      query(
        union(
          singleQuery(use(varFor("foo"))),
          singleQuery(return_(returnItem(literal(1), "1")))
        ).all
      )
    }
  }
}
