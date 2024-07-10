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

import org.opencypher.v9_0.ast.Clause
import org.opencypher.v9_0.ast.SingleQuery
import org.opencypher.v9_0.ast.SubqueryCall
import org.opencypher.v9_0.ast.SubqueryCall.InTransactionsParameters
import org.opencypher.v9_0.util.symbols.CTAny

class CypherTransactionsParserTest extends JavaccParserAstTestBase[Clause] with VerifyAstPositionTestSupport {

  implicit private val parser: JavaccRule[Clause] = JavaccRule.Clause

  test("CALL { CREATE (n) } IN TRANSACTIONS") {
    val expected =
      SubqueryCall(
        SingleQuery(
          Seq(create(
            nodePat(Some("n"), namePos = (1, 16, 15), position = (1, 15, 14)),
            (1, 8, 7)
          ))
        )(defaultPos),
        Nil,
        null,
        Some(InTransactionsParameters(None)((1, 21, 20)))
      )(defaultPos)

    parsing(testName) shouldVerify { actual =>
      actual shouldBe expected
      verifyPositions(actual, expected)
    }
  }

  test("CALL { CREATE (n) } IN TRANSACTIONS OF 1 ROW") {
    val expected = subqueryCallInTransactions(inTransactionsParameters(Some(literalInt(1))), create(nodePat(Some("n"))))
    gives(expected)
  }

  test("CALL { CREATE (n) } IN TRANSACTIONS OF 1 ROWS") {
    val expected = subqueryCallInTransactions(inTransactionsParameters(Some(literalInt(1))), create(nodePat(Some("n"))))
    gives(expected)
  }

  test("CALL { CREATE (n) } IN TRANSACTIONS OF 42 ROW") {
    val expected =
      subqueryCallInTransactions(inTransactionsParameters(Some(literalInt(42))), create(nodePat(Some("n"))))
    gives(expected)
  }

  test("CALL { CREATE (n) } IN TRANSACTIONS OF 42 ROWS") {
    val expected =
      subqueryCallInTransactions(inTransactionsParameters(Some(literalInt(42))), create(nodePat(Some("n"))))
    gives(expected)
  }

  test("CALL { CREATE (n) } IN TRANSACTIONS OF $param ROWS") {
    val expected =
      subqueryCallInTransactions(inTransactionsParameters(Some(parameter("param", CTAny))), create(nodePat(Some("n"))))
    gives(expected)
  }

  test("CALL { CREATE (n) } IN TRANSACTIONS OF NULL ROWS") {
    val expected = subqueryCallInTransactions(inTransactionsParameters(Some(nullLiteral)), create(nodePat(Some("n"))))
    gives(expected)
  }
}
