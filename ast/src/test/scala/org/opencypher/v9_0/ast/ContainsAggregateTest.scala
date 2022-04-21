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

import org.opencypher.v9_0.expressions.CountStar
import org.opencypher.v9_0.expressions.containsAggregate
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ContainsAggregateTest extends CypherFunSuite with AstConstructionTestSupport {

  test("finds nested aggregate expressions") {
    val expr = add(literalInt(1), CountStar() _)

    containsAggregate(expr) should equal(true)
  }

  test("does not match non-aggregate expressions") {
    val expr = add(literalInt(1), literalInt(2))

    containsAggregate(expr) should equal(false)
  }
}
