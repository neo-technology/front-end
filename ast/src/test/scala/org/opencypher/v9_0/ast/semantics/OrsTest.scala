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
package org.opencypher.v9_0.ast.semantics

import org.opencypher.v9_0.expressions.DummyExpression
import org.opencypher.v9_0.expressions.Ors
import org.opencypher.v9_0.util.DummyPosition
import org.opencypher.v9_0.util.symbols.CTBoolean

class OrsTest extends SemanticFunSuite {

  test("should semantic check all expressions in ors") {
    val dummyExpr1 = DummyExpression(CTBoolean, DummyPosition(1))
    val dummyExpr2 = DummyExpression(CTBoolean, DummyPosition(2))
    val dummyExpr3 = DummyExpression(CTBoolean, DummyPosition(3))
    val ors = Ors(Seq(dummyExpr1, dummyExpr2, dummyExpr3))(pos)
    val result = SemanticExpressionCheck.simple(ors)(SemanticState.clean)

    result.errors shouldBe empty
    result.state.typeTable.keySet.map(_.node) should contain allOf (dummyExpr1, dummyExpr2, dummyExpr3, ors)
  }
}
