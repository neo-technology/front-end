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

class PeriodicCommitParserTest extends JavaccParserAstTestBase[Statement] {

  implicit private val parser: JavaccRule[Statement] = JavaccRule.Statement

  val message =
    "The PERIODIC COMMIT query hint is no longer supported. Please use CALL { ... } IN TRANSACTIONS instead. (line 1, column 7 (offset: 6))"

  test("USING PERIODIC COMMIT LOAD CSV FROM 'foo' AS l RETURN l") {
    assertFailsWithMessage(testName, message)
  }

  test("USING PERIODIC COMMIT 200 LOAD CSV FROM 'foo' AS l RETURN l") {
    assertFailsWithMessage(testName, message)
  }

  test("USING PERIODIC COMMIT RETURN 1") {
    assertFailsWithMessage(testName, message)
  }
}
