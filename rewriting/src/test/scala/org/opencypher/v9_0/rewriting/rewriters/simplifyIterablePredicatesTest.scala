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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.rewriting.AstRewritingTestSupport
import org.opencypher.v9_0.rewriting.RewriteTest
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite
import org.scalatest.Matchers

class simplifyIterablePredicatesTest extends CypherFunSuite with Matchers with RewriteTest with AstRewritingTestSupport {

  test("should rewrite simple any with literal") {
    assertRewrite(
      "WITH [1,2,3] AS list RETURN any(x IN list WHERE x = 2) AS result",
      "WITH [1,2,3] AS list RETURN 2 IN list AS result"
    )
    assertRewrite(
      "WITH [1,2,3] AS list RETURN any(x IN list WHERE 2 = x) AS result",
      "WITH [1,2,3] AS list RETURN 2 IN list AS result"
    )
  }

  test("should rewrite simple any with in and single literal") {
    assertRewrite(
      "WITH [1,2,3] AS list RETURN any(x IN list WHERE x IN [2]) AS result",
      "WITH [1,2,3] AS list RETURN 2 IN list AS result"
    )
  }

  test("should rewrite simple any with variable") {
    assertRewrite(
      "WITH [1,2,3] AS list, 1 AS a RETURN any(x IN list WHERE x = a) AS result",
      "WITH [1,2,3] AS list, 1 AS a RETURN a IN list AS result"
    )
  }

  test("should not rewrite when inner predicate is not equals") {
    assertIsNotRewritten("WITH [1,2,3] AS list RETURN any(x IN list WHERE x > 2) AS result")
    assertIsNotRewritten("WITH [1,2,3] AS list RETURN any(x IN list WHERE 2 < x) AS result")
    assertIsNotRewritten("WITH [1,2,3] AS list RETURN any(x IN list WHERE x IN [1,2]) AS result")
  }

  test("should not rewrite queries that depends on the scope variable on both sides") {
    assertIsNotRewritten("MATCH (n) RETURN any(x IN n.list WHERE x = (2*x)) AS result")
    assertIsNotRewritten("MATCH (n) RETURN any(x IN n.a WHERE x = x.b) AS result")
  }

  test("should not rewrite queries that uses the scope variable in any way other than for equality") {
    assertIsNotRewritten("MATCH (n) RETURN any(x IN n.list WHERE x.a = 1) AS result")
    assertIsNotRewritten("MATCH (n) RETURN any(x IN n.list WHERE 1 = x.a) AS result")
    assertIsNotRewritten("MATCH (n) RETURN any(x IN n.list WHERE size(a) > 0) AS result")
  }

  // The following tests can't be written in RewriteTest style because there is no way to express Not.

  test("should rewrite none(x in list WHERE x = 2) to not(2 IN list)") {
    val expr = noneInList(
      varFor("x"),
      varFor("list"),
      equals(varFor("x"), literal(2))
    )
    rewrite(expr) shouldBe not(in(literal(2), varFor("list")))
  }

  test("should rewrite none(x in [1, 2, 3] WHERE 2 = x) to 2 IN [1, 2, 3]") {
    val expr = noneInList(
      varFor("x"),
      varFor("list"),
      equals(literal(2), varFor("x"))
    )
    rewrite(expr) shouldBe not(in(literal(2), varFor("list")))
  }

  test("should rewrite none(x in list WHERE x = a) to not(a IN list)") {
    val expr = noneInList(
      varFor("x"),
      varFor("list"),
      equals(varFor("x"), varFor("a"))
    )
    rewrite(expr) shouldBe not(in(varFor("a"), varFor("list")))
  }

  test("should not rewrite none(x in list WHERE x > 1)") {
    val expr = noneInList(
      varFor("x"),
      varFor("list"),
      greaterThan(varFor("x"), literalInt(1))
    )
    rewrite(expr) shouldBe expr
  }

  test("should not rewrite none(x in list WHERE x = x.b)") {
    val expr = noneInList(
      varFor("x"),
      varFor("list"),
      equals(varFor("x"), prop(varFor("x"), "a"))
    )
    rewrite(expr) shouldBe expr
  }

  test("should not rewrite none(x in list WHERE x.a = 1)") {
    val expr = noneInList(
      varFor("x"),
      varFor("list"),
      equals(prop(varFor("x"), "a"), literalInt(1))
    )
    rewrite(expr) shouldBe expr
  }

  override def rewriterUnderTest: Rewriter = simplifyIterablePredicates

  private def rewrite(e: Expression): Expression = e.endoRewrite(simplifyIterablePredicates)
}
