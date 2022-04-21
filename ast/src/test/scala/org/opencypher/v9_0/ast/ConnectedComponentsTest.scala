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
package org.opencypher.v9_0.ast

import org.opencypher.v9_0.ast.connectedComponents.ComponentPart
import org.opencypher.v9_0.ast.connectedComponents.ConnectedComponent
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ConnectedComponentsTest extends CypherFunSuite {

  test("(a)->(b), (c)->(d) has two connected components") {
    val disconnected = connectedComponents(Vector(
      ComponentPart(varFor("a"), varFor("b")),
      ComponentPart(varFor("c"), varFor("d"))
    ))

    disconnected should equal(Vector(
      ConnectedComponent(ComponentPart(varFor("a"), varFor("b"))),
      ConnectedComponent(ComponentPart(varFor("c"), varFor("d")))
    ))
  }

  test("(a)->(b)->(c) does contain one connected component") {
    val disconnected = connectedComponents(Vector(
      ComponentPart(varFor("a"), varFor("b")),
      ComponentPart(varFor("b"), varFor("c"))
    ))

    disconnected should equal(Vector(
      ConnectedComponent(ComponentPart(varFor("a"), varFor("b")), ComponentPart(varFor("b"), varFor("c")))
    ))
  }

  test("(a)->(b)->(c)->(d) does only contain one component") {
    val disconnected = connectedComponents(Vector(
      ComponentPart(varFor("a"), varFor("b")),
      ComponentPart(varFor("b"), varFor("c")),
      ComponentPart(varFor("c"), varFor("d"))
    ))

    disconnected shouldBe Vector(ConnectedComponent(
      ComponentPart(varFor("a"), varFor("b")),
      ComponentPart(varFor("b"), varFor("c")),
      ComponentPart(varFor("c"), varFor("d"))
    ))
  }

  test("(a)->(b)->(c)-(a) contains one component ") {
    val disconnected = connectedComponents(Vector(
      ComponentPart(varFor("a"), varFor("b")),
      ComponentPart(varFor("b"), varFor("c")),
      ComponentPart(varFor("c"), varFor("a"))
    ))

    disconnected shouldBe Vector(ConnectedComponent(
      ComponentPart(varFor("a"), varFor("b")),
      ComponentPart(varFor("b"), varFor("c")),
      ComponentPart(varFor("c"), varFor("a"))
    ))
  }

  private def varFor(name: String): LogicalVariable = Variable(name)(null)
}
