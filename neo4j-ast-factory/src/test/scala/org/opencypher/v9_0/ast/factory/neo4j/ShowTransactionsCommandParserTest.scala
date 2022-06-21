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

import org.opencypher.v9_0.ast
import org.opencypher.v9_0.util.symbols.CTAny

/* Tests for listing transactions */
class ShowTransactionsCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {

  Seq("TRANSACTION", "TRANSACTIONS").foreach { transactionKeyword =>
    test(s"SHOW $transactionKeyword") {
      assertAst(query(ast.ShowTransactionsClause(Left(List.empty), None, List.empty, yieldAll = false)(defaultPos)))
    }

    test(s"SHOW $transactionKeyword 'db1-transaction-123'") {
      assertAst(
        query(
          ast.ShowTransactionsClause(
            Right(literalString("db1-transaction-123")),
            None,
            List.empty,
            yieldAll = false
          )(defaultPos)
        )
      )
    }

    test(s"""SHOW $transactionKeyword "db1-transaction-123"""") {
      assertAst(
        query(
          ast.ShowTransactionsClause(
            Right(literalString("db1-transaction-123")),
            None,
            List.empty,
            yieldAll = false
          )(defaultPos)
        )
      )
    }

    test(s"SHOW $transactionKeyword 'my.db-transaction-123'") {
      assertAst(
        query(ast.ShowTransactionsClause(
          Right(literalString("my.db-transaction-123")),
          None,
          List.empty,
          yieldAll = false
        )(defaultPos))
      )
    }

    test(s"SHOW $transactionKeyword $$param") {
      assertAst(query(ast.ShowTransactionsClause(
        Right(parameter("param", CTAny)),
        None,
        List.empty,
        yieldAll = false
      )(defaultPos)))
    }

    test(s"SHOW $transactionKeyword $$where") {
      assertAst(query(ast.ShowTransactionsClause(
        Right(parameter("where", CTAny)),
        None,
        List.empty,
        yieldAll = false
      )(defaultPos)))
    }

    test(s"""SHOW $transactionKeyword 'db1 - transaction - 123', "db2-transaction-45a6"""") {
      assertAst(query(ast.ShowTransactionsClause(
        Left(List("db1 - transaction - 123", "db2-transaction-45a6")),
        None,
        List.empty,
        yieldAll = false
      )(defaultPos)))
    }

    test(s"SHOW $transactionKeyword 'yield-transaction-123'") {
      assertAst(
        query(ast.ShowTransactionsClause(
          Right(literalString("yield-transaction-123")),
          None,
          List.empty,
          yieldAll = false
        )(defaultPos))
      )
    }

    test(s"SHOW $transactionKeyword 'where-transaction-123'") {
      assertAst(
        query(ast.ShowTransactionsClause(
          Right(literalString("where-transaction-123")),
          None,
          List.empty,
          yieldAll = false
        )(defaultPos))
      )
    }

    test(s"USE db SHOW $transactionKeyword") {
      assertAst(
        query(use(varFor("db")), ast.ShowTransactionsClause(Left(List.empty), None, List.empty, yieldAll = false)(pos)),
        comparePosition = false
      )
    }

  }

  test("SHOW TRANSACTION db-transaction-123") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Right(subtract(subtract(varFor("db"), varFor("transaction")), literalInt(123))),
        None,
        List.empty,
        yieldAll = false
      )(pos)
    ))
  }

  test("SHOW TRANSACTION 'neo4j'+'-transaction-'+3") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Right(add(add(literalString("neo4j"), literalString("-transaction-")), literalInt(3))),
        None,
        List.empty,
        yieldAll = false
      )(pos)
    ))
  }

  test("SHOW TRANSACTION ('neo4j'+'-transaction-'+3)") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Right(add(add(literalString("neo4j"), literalString("-transaction-")), literalInt(3))),
        None,
        List.empty,
        yieldAll = false
      )(pos)
    ))
  }

  test("SHOW TRANSACTIONS ['db1-transaction-123', 'db2-transaction-456']") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Right(listOfString("db1-transaction-123", "db2-transaction-456")),
        None,
        List.empty,
        yieldAll = false
      )(pos)
    ))
  }

  test("SHOW TRANSACTION foo") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("foo")), None, List.empty, yieldAll = false)(pos)
    ))
  }

  test("SHOW TRANSACTION x+2") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(add(varFor("x"), literalInt(2))), None, List.empty, yieldAll = false)(pos)
    ))
  }

  test("SHOW TRANSACTIONS YIELD") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("YIELD")), None, List.empty, yieldAll = false)(pos)
    ))
  }

  test("SHOW TRANSACTIONS YIELD (123 + xyz)") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Right(function("YIELD", add(literalInt(123), varFor("xyz")))),
        None,
        List.empty,
        yieldAll = false
      )(pos)
    ))
  }

  test("SHOW TRANSACTIONS ALL") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("ALL")), None, List.empty, yieldAll = false)(pos)
    ))
  }

  // Filtering tests

  test("SHOW TRANSACTION WHERE transactionId = 'db1-transaction-123'") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Left(List.empty),
        Some(where(equals(varFor("transactionId"), literalString("db1-transaction-123")))),
        List.empty,
        yieldAll = false
      )(defaultPos)
    ))
  }

  test("SHOW TRANSACTIONS YIELD database") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Left(List.empty),
          None,
          List(commandResultItem("database", Some("database"))),
          yieldAll = false
        )(pos),
        withFromYield(returnAllItems.withDefaultOrderOnColumns(List("database")))
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS 'db1-transaction-123', 'db2-transaction-456' YIELD *") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Left(List("db1-transaction-123", "db2-transaction-456")),
        None,
        List.empty,
        yieldAll = true
      )(
        defaultPos
      ),
      withFromYield(returnAllItems)
    ))
  }

  test("SHOW TRANSACTIONS 'db1-transaction-123', 'db2-transaction-456', 'yield' YIELD *") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Left(List("db1-transaction-123", "db2-transaction-456", "yield")),
          None,
          List.empty,
          yieldAll = true
        )(pos),
        withFromYield(returnAllItems)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS YIELD * ORDER BY transactionId SKIP 2 LIMIT 5") {
    assertAst(
      query(
        ast.ShowTransactionsClause(Left(List.empty), None, List.empty, yieldAll = true)(pos),
        withFromYield(returnAllItems, Some(orderBy(sortItem(varFor("transactionId")))), Some(skip(2)), Some(limit(5)))
      ),
      comparePosition = false
    )
  }

  test("USE db SHOW TRANSACTIONS YIELD transactionId, activeLockCount AS pp WHERE pp < 50 RETURN transactionId") {
    assertAst(
      query(
        use(varFor("db")),
        ast.ShowTransactionsClause(
          Left(List.empty),
          None,
          List(
            commandResultItem("transactionId", Some("transactionId")),
            commandResultItem("activeLockCount", Some("pp"))
          ),
          yieldAll = false
        )(pos),
        withFromYield(
          returnAllItems.withDefaultOrderOnColumns(List("transactionId", "pp")),
          where = Some(where(lessThan(varFor("pp"), literalInt(50L))))
        ),
        return_(variableReturnItem("transactionId"))
      ),
      comparePosition = false
    )
  }

  test(
    "USE db SHOW TRANSACTIONS YIELD transactionId, activeLockCount AS pp ORDER BY pp SKIP 2 LIMIT 5 WHERE pp < 50 RETURN transactionId"
  ) {
    assertAst(
      query(
        use(varFor("db")),
        ast.ShowTransactionsClause(
          Left(List.empty),
          None,
          List(
            commandResultItem("transactionId", Some("transactionId")),
            commandResultItem("activeLockCount", Some("pp"))
          ),
          yieldAll = false
        )(pos),
        withFromYield(
          returnAllItems.withDefaultOrderOnColumns(List("transactionId", "pp")),
          Some(orderBy(sortItem(varFor("pp")))),
          Some(skip(2)),
          Some(limit(5)),
          Some(where(lessThan(varFor("pp"), literalInt(50L))))
        ),
        return_(variableReturnItem("transactionId"))
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS $param YIELD transactionId AS TRANSACTION, database AS OUTPUT") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(parameter("param", CTAny)),
          None,
          List(commandResultItem("transactionId", Some("TRANSACTION")), commandResultItem("database", Some("OUTPUT"))),
          yieldAll = false
        )(pos),
        withFromYield(returnAllItems.withDefaultOrderOnColumns(List("TRANSACTION", "OUTPUT")))
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS 'where' YIELD transactionId AS TRANSACTION, database AS OUTPUT") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(literalString("where")),
          None,
          List(commandResultItem("transactionId", Some("TRANSACTION")), commandResultItem("database", Some("OUTPUT"))),
          yieldAll = false
        )(pos),
        withFromYield(returnAllItems.withDefaultOrderOnColumns(List("TRANSACTION", "OUTPUT")))
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTION 'db1-transaction-123' WHERE transactionId = 'db1-transaction-124'") {
    assertAst(
      query(ast.ShowTransactionsClause(
        Right(literalString("db1-transaction-123")),
        Some(where(equals(varFor("transactionId"), literalString("db1-transaction-124")))),
        List.empty,
        yieldAll = false
      )(pos)),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTION 'yield' WHERE transactionId = 'where'") {
    assertAst(
      query(ast.ShowTransactionsClause(
        Right(literalString("yield")),
        Some(where(equals(varFor("transactionId"), literalString("where")))),
        List.empty,
        yieldAll = false
      )(pos)),
      comparePosition = false
    )
  }

  test(
    "SHOW TRANSACTION 'db1-transaction-123', 'db1-transaction-124' WHERE transactionId IN ['db1-transaction-124', 'db1-transaction-125']"
  ) {
    assertAst(
      query(ast.ShowTransactionsClause(
        Left(List("db1-transaction-123", "db1-transaction-124")),
        Some(where(in(varFor("transactionId"), listOfString("db1-transaction-124", "db1-transaction-125")))),
        List.empty,
        yieldAll = false
      )(pos)),
      comparePosition = false
    )
  }

  test(
    "SHOW TRANSACTION db1-transaction-123 WHERE transactionId IN ['db1-transaction-124', 'db1-transaction-125']"
  ) {
    assertAst(
      query(ast.ShowTransactionsClause(
        Right(subtract(subtract(varFor("db1"), varFor("transaction")), literalInt(123))),
        Some(where(in(varFor("transactionId"), listOfString("db1-transaction-124", "db1-transaction-125")))),
        List.empty,
        yieldAll = false
      )(pos)),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS ['db1-transaction-123', 'db2-transaction-456'] YIELD *") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(listOfString("db1-transaction-123", "db2-transaction-456")),
          None,
          List.empty,
          yieldAll = true
        )(pos),
        withFromYield(returnAllItems)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS $x+'123' YIELD transactionId AS TRANSACTION, database AS SHOW") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(add(parameter("x", CTAny), literalString("123"))),
          None,
          List(commandResultItem("transactionId", Some("TRANSACTION")), commandResultItem("database", Some("SHOW"))),
          yieldAll = false
        )(pos),
        withFromYield(returnAllItems.withDefaultOrderOnColumns(List("TRANSACTION", "SHOW")))
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS where YIELD *") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("where")),
          None,
          List.empty,
          yieldAll = true
        )(pos),
        withFromYield(returnAllItems)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS yield YIELD *") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("yield")),
          None,
          List.empty,
          yieldAll = true
        )(pos),
        withFromYield(returnAllItems)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS show YIELD *") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("show")),
          None,
          List.empty,
          yieldAll = true
        )(pos),
        withFromYield(returnAllItems)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS terminate YIELD *") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("terminate")),
          None,
          List.empty,
          yieldAll = true
        )(pos),
        withFromYield(returnAllItems)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS YIELD yield") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Left(List.empty),
          None,
          List(commandResultItem("yield", Some("yield"))),
          yieldAll = false
        )(pos),
        withFromYield(returnAllItems.withDefaultOrderOnColumns(List("yield")))
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS where WHERE true") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("where")),
          Some(where(trueLiteral)),
          List.empty,
          yieldAll = false
        )(pos)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS yield WHERE true") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("yield")),
          Some(where(trueLiteral)),
          List.empty,
          yieldAll = false
        )(pos)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS show WHERE true") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("show")),
          Some(where(trueLiteral)),
          List.empty,
          yieldAll = false
        )(pos)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS terminate WHERE true") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("terminate")),
          Some(where(trueLiteral)),
          List.empty,
          yieldAll = false
        )(pos)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS `yield` YIELD *") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("yield")),
          None,
          List.empty,
          yieldAll = true
        )(pos),
        withFromYield(returnAllItems)
      ),
      comparePosition = false
    )
  }

  test("SHOW TRANSACTIONS `where` WHERE true") {
    assertAst(
      query(
        ast.ShowTransactionsClause(
          Right(varFor("where")),
          Some(where(trueLiteral)),
          List.empty,
          yieldAll = false
        )(pos)
      ),
      comparePosition = false
    )
  }

  // Negative tests

  test("SHOW TRANSACTION db-transaction-123, abc") {
    failsToParse
  }

  test("SHOW TRANSACTIONS 'db-transaction-123', $param") {
    failsToParse
  }

  test("SHOW TRANSACTIONS $param, 'db-transaction-123'") {
    failsToParse
  }

  test("SHOW TRANSACTIONS $param, $param2") {
    failsToParse
  }

  test("SHOW TRANSACTIONS ['db1-transaction-123', 'db2-transaction-456'], abc") {
    failsToParse
  }

  test("SHOW TRANSACTION foo, 'abc'") {
    failsToParse
  }

  test("SHOW TRANSACTION x+2, abc") {
    failsToParse
  }

  test("SHOW TRANSACTIONS YIELD * YIELD *") {
    failsToParse
  }

  test("SHOW TRANSACTIONS YIELD (123 + xyz) AS foo") {
    failsToParse
  }

  test("SHOW TRANSACTIONS WHERE transactionId = 'db1-transaction-123' YIELD *") {
    failsToParse
  }

  test("SHOW TRANSACTIONS WHERE transactionId = 'db1-transaction-123' RETURN *") {
    failsToParse
  }

  test("SHOW TRANSACTIONS YIELD a b RETURN *") {
    failsToParse
  }

  test("SHOW TRANSACTIONS RETURN *") {
    failsToParse
  }

  test("SHOW CURRENT USER TRANSACTION") {
    failsToParse
  }

  test("SHOW USER user TRANSACTION") {
    assertFailsWithMessage(
      testName,
      """Invalid input 'TRANSACTION': expected ",", "PRIVILEGE" or "PRIVILEGES" (line 1, column 16 (offset: 15))""".stripMargin
    )
  }

  test("SHOW TRANSACTION EXECUTED BY USER user") {
    failsToParse
  }

  test("SHOW ALL TRANSACTIONS") {
    failsToParse
  }

  // Invalid clause order

  for (prefix <- Seq("USE neo4j", "")) {
    test(s"$prefix SHOW TRANSACTIONS YIELD * WITH * MATCH (n) RETURN n") {
      // Can't parse WITH after SHOW
      assertFailsWithMessageStart(testName, "Invalid input 'WITH': expected")
    }

    test(s"$prefix UNWIND range(1,10) as b SHOW TRANSACTIONS YIELD * RETURN *") {
      // Can't parse SHOW  after UNWIND
      assertFailsWithMessageStart(testName, "Invalid input 'SHOW': expected")
    }

    test(s"$prefix SHOW TRANSACTIONS WITH name, type RETURN *") {
      // Can't parse WITH after SHOW
      // parses varFor("WITH")
      assertFailsWithMessageStart(testName, "Invalid input 'name': expected")
    }

    test(s"$prefix WITH 'n' as n SHOW TRANSACTIONS YIELD name RETURN name as numIndexes") {
      assertFailsWithMessageStart(testName, "Invalid input 'SHOW': expected")
    }

    test(s"$prefix SHOW TRANSACTIONS RETURN name as numIndexes") {
      // parses varFor("RETURN")
      assertFailsWithMessageStart(testName, "Invalid input 'name': expected")
    }

    test(s"$prefix SHOW TRANSACTIONS WITH 1 as c RETURN name as numIndexes") {
      // parses varFor("WITH")
      assertFailsWithMessageStart(testName, "Invalid input '1': expected")
    }

    test(s"$prefix SHOW TRANSACTIONS WITH 1 as c") {
      // parses varFor("WITH")
      assertFailsWithMessageStart(testName, "Invalid input '1': expected")
    }

    test(s"$prefix SHOW TRANSACTIONS YIELD a WITH a RETURN a") {
      assertFailsWithMessageStart(testName, "Invalid input 'WITH': expected")
    }

    test(s"$prefix SHOW TRANSACTIONS YIELD as UNWIND as as a RETURN a") {
      assertFailsWithMessageStart(testName, "Invalid input 'UNWIND': expected")
    }

    test(s"$prefix SHOW TRANSACTIONS RETURN id2 YIELD id2") {
      // parses varFor("RETURN")
      assertFailsWithMessageStart(testName, "Invalid input 'id2': expected")
    }
  }

  // Brief/verbose not allowed

  test("SHOW TRANSACTION BRIEF") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("BRIEF")), None, List.empty, yieldAll = false)(pos)
    ))
  }

  test("SHOW TRANSACTIONS BRIEF YIELD *") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("BRIEF")), None, List.empty, yieldAll = true)(pos),
      withFromYield(returnAllItems)
    ))
  }

  test("SHOW TRANSACTIONS BRIEF WHERE transactionId = 'db1-transaction-123'") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Right(varFor("BRIEF")),
        Some(where(equals(varFor("transactionId"), literalString("db1-transaction-123")))),
        List.empty,
        yieldAll = false
      )(pos)
    ))
  }

  test("SHOW TRANSACTION VERBOSE") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("VERBOSE")), None, List.empty, yieldAll = false)(pos)
    ))
  }

  test("SHOW TRANSACTIONS VERBOSE YIELD *") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("VERBOSE")), None, List.empty, yieldAll = true)(pos),
      withFromYield(returnAllItems)
    ))
  }

  test("SHOW TRANSACTIONS VERBOSE WHERE transactionId = 'db1-transaction-123'") {
    assertAst(query(
      ast.ShowTransactionsClause(
        Right(varFor("VERBOSE")),
        Some(where(equals(varFor("transactionId"), literalString("db1-transaction-123")))),
        List.empty,
        yieldAll = false
      )(pos)
    ))
  }

  test("SHOW TRANSACTION OUTPUT") {
    assertAst(query(
      ast.ShowTransactionsClause(Right(varFor("OUTPUT")), None, List.empty, yieldAll = false)(pos)
    ))
  }

  test("SHOW TRANSACTION BRIEF OUTPUT") {
    failsToParse
  }

  test("SHOW TRANSACTIONS BRIEF RETURN *") {
    failsToParse
  }

  test("SHOW TRANSACTION VERBOSE OUTPUT") {
    failsToParse
  }

  test("SHOW TRANSACTIONS VERBOSE RETURN *") {
    failsToParse
  }

}
