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
import org.opencypher.v9_0.ast.AllDatabasesScope
import org.opencypher.v9_0.ast.AlterDatabase
import org.opencypher.v9_0.ast.CreateDatabase
import org.opencypher.v9_0.ast.DefaultDatabaseScope
import org.opencypher.v9_0.ast.DestroyData
import org.opencypher.v9_0.ast.DumpData
import org.opencypher.v9_0.ast.HomeDatabaseScope
import org.opencypher.v9_0.ast.IfExistsThrowError
import org.opencypher.v9_0.ast.IndefiniteWait
import org.opencypher.v9_0.ast.NamedDatabaseScope
import org.opencypher.v9_0.ast.NoOptions
import org.opencypher.v9_0.ast.NoWait
import org.opencypher.v9_0.ast.OptionsMap
import org.opencypher.v9_0.ast.OptionsParam
import org.opencypher.v9_0.ast.ReadOnlyAccess
import org.opencypher.v9_0.ast.ReadWriteAccess
import org.opencypher.v9_0.ast.TimeoutAfter
import org.opencypher.v9_0.ast.YieldOrWhere
import org.opencypher.v9_0.expressions
import org.opencypher.v9_0.expressions.Parameter
import org.opencypher.v9_0.expressions.StringLiteral
import org.opencypher.v9_0.util.symbols.CTMap
import org.opencypher.v9_0.util.symbols.CTString

class MultiDatabaseAdministrationCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {
  private val literalFooBar = literal("foo.bar")

  // SHOW DATABASE

  Seq(
    ("DATABASES", ast.ShowDatabase.apply(AllDatabasesScope()(pos), _: YieldOrWhere) _),
    ("DEFAULT DATABASE", ast.ShowDatabase.apply(DefaultDatabaseScope()(pos), _: YieldOrWhere) _),
    ("HOME DATABASE", ast.ShowDatabase.apply(HomeDatabaseScope()(pos), _: YieldOrWhere) _),
    ("DATABASE $db", ast.ShowDatabase.apply(NamedDatabaseScope(param("db"))(pos), _: YieldOrWhere) _),
    ("DATABASE neo4j", ast.ShowDatabase.apply(NamedDatabaseScope(literal("neo4j"))(pos), _: YieldOrWhere) _)
  ).foreach { case (dbType, privilege) =>
    test(s"SHOW $dbType") {
      yields(privilege(None))
    }

    test(s"USE system SHOW $dbType") {
      yields(privilege(None))
    }

    test(s"SHOW $dbType WHERE access = 'GRANTED'") {
      yields(privilege(Some(Right(where(equals(accessVar, grantedString))))))
    }

    test(s"SHOW $dbType WHERE access = 'GRANTED' AND action = 'match'") {
      val accessPredicate = equals(accessVar, grantedString)
      val matchPredicate = equals(varFor(actionString), literalString("match"))
      yields(privilege(Some(Right(where(and(accessPredicate, matchPredicate))))))
    }

    test(s"SHOW $dbType YIELD access ORDER BY access") {
      val orderByClause = orderBy(sortItem(accessVar))
      val columns = yieldClause(returnItems(variableReturnItem(accessString)), Some(orderByClause))
      yields(privilege(Some(Left((columns, None)))))
    }

    test(s"SHOW $dbType YIELD access ORDER BY access WHERE access ='none'") {
      val orderByClause = orderBy(sortItem(accessVar))
      val whereClause = where(equals(accessVar, noneString))
      val columns =
        yieldClause(returnItems(variableReturnItem(accessString)), Some(orderByClause), where = Some(whereClause))
      yields(privilege(Some(Left((columns, None)))))
    }

    test(s"SHOW $dbType YIELD access ORDER BY access SKIP 1 LIMIT 10 WHERE access ='none'") {
      val orderByClause = orderBy(sortItem(accessVar))
      val whereClause = where(equals(accessVar, noneString))
      val columns = yieldClause(
        returnItems(variableReturnItem(accessString)),
        Some(orderByClause),
        Some(skip(1)),
        Some(limit(10)),
        Some(whereClause)
      )
      yields(privilege(Some(Left((columns, None)))))
    }

    test(s"SHOW $dbType YIELD access SKIP -1") {
      val columns = yieldClause(returnItems(variableReturnItem(accessString)), skip = Some(skip(-1)))
      yields(privilege(Some(Left((columns, None)))))
    }

    test(s"SHOW $dbType YIELD access ORDER BY access RETURN access") {
      yields(privilege(
        Some(Left((
          yieldClause(returnItems(variableReturnItem(accessString)), Some(orderBy(sortItem(accessVar)))),
          Some(returnClause(returnItems(variableReturnItem(accessString))))
        )))
      ))
    }

    test(s"SHOW $dbType WHERE access = 'GRANTED' RETURN action") {
      failsToParse
    }

    test(s"SHOW $dbType YIELD * RETURN *") {
      yields(privilege(Some(Left((yieldClause(returnAllItems), Some(returnClause(returnAllItems)))))))
    }
  }

  test("SHOW DATABASE `foo.bar`") {
    yields(ast.ShowDatabase(NamedDatabaseScope(literalFooBar)(pos), None))
  }

  test("SHOW DATABASE foo.bar") {
    yields(ast.ShowDatabase(NamedDatabaseScope(literalFooBar)(pos), None))
  }

  test("SHOW DATABASE") {
    assertFailsWithMessage(
      testName,
      "Invalid input '': expected a parameter or an identifier (line 1, column 14 (offset: 13))"
    )
  }

  test("SHOW DATABASE blah YIELD *,blah RETURN user") {
    failsToParse
  }

  test("SHOW DATABASE YIELD (123 + xyz)") {
    failsToParse
  }

  test("SHOW DATABASE YIELD (123 + xyz) AS foo") {
    failsToParse
  }

  // CREATE DATABASE

  test("CREATE DATABASE foo") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("USE system CREATE DATABASE foo") {
    // can parse USE clause, but is not included in AST
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("CREATE DATABASE $foo") {
    yields(ast.CreateDatabase(paramFoo, ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("CREATE DATABASE $wait") {
    yields(ast.CreateDatabase(param("wait"), ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("CREATE DATABASE `nowait.sec`") {
    yields(ast.CreateDatabase(literal("nowait.sec"), ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("CREATE DATABASE second WAIT") {
    yields(ast.CreateDatabase(literal("second"), ast.IfExistsThrowError, NoOptions, IndefiniteWait))
  }

  test("CREATE DATABASE seconds WAIT 12") {
    yields(ast.CreateDatabase(literal("seconds"), ast.IfExistsThrowError, NoOptions, TimeoutAfter(12)))
  }

  test("CREATE DATABASE dump WAIT 12 SEC") {
    yields(ast.CreateDatabase(literal("dump"), ast.IfExistsThrowError, NoOptions, TimeoutAfter(12)))
  }

  test("CREATE DATABASE destroy WAIT 12 SECOND") {
    yields(ast.CreateDatabase(literal("destroy"), ast.IfExistsThrowError, NoOptions, TimeoutAfter(12)))
  }

  test("CREATE DATABASE data WAIT 12 SECONDS") {
    yields(ast.CreateDatabase(literal("data"), ast.IfExistsThrowError, NoOptions, TimeoutAfter(12)))
  }

  test("CREATE DATABASE foo NOWAIT") {
    yields(ast.CreateDatabase(literal("foo"), ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("CREATE DATABASE `foo.bar`") {
    yields(ast.CreateDatabase(literalFooBar, ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("CREATE DATABASE foo.bar") {
    yields(ast.CreateDatabase(literalFooBar, ast.IfExistsThrowError, NoOptions, NoWait))
  }

  test("CREATE DATABASE `graph.db`.`db.db`") {
    yields(_ => ast.CreateDatabase(literal("graph.db.db.db"), ast.IfExistsThrowError, NoOptions, NoWait)(pos))
  }

  test("CREATE DATABASE `foo-bar42`") {
    yields(_ => ast.CreateDatabase(literal("foo-bar42"), ast.IfExistsThrowError, NoOptions, NoWait)(pos))
  }

  test("CREATE DATABASE `_foo-bar42`") {
    yields(_ => ast.CreateDatabase(literal("_foo-bar42"), ast.IfExistsThrowError, NoOptions, NoWait)(pos))
  }

  test("CREATE DATABASE ``") {
    yields(_ => ast.CreateDatabase(literalEmpty, ast.IfExistsThrowError, NoOptions, NoWait)(pos))
  }

  test("CREATE DATABASE foo IF NOT EXISTS") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsDoNothing, NoOptions, NoWait))
  }

  test("CREATE DATABASE foo IF NOT EXISTS WAIT 10 SECONDS") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsDoNothing, NoOptions, TimeoutAfter(10)))
  }

  test("CREATE DATABASE foo IF NOT EXISTS WAIT") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsDoNothing, NoOptions, IndefiniteWait))
  }

  test("CREATE  DATABASE foo IF NOT EXISTS NOWAIT") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsDoNothing, NoOptions, NoWait))
  }

  test("CREATE DATABASE `_foo-bar42` IF NOT EXISTS") {
    yields(_ => ast.CreateDatabase(literal("_foo-bar42"), ast.IfExistsDoNothing, NoOptions, NoWait)(pos))
  }

  test("CREATE OR REPLACE DATABASE foo") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsReplace, NoOptions, NoWait))
  }

  test("CREATE OR REPLACE DATABASE foo WAIT 10 SECONDS") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsReplace, NoOptions, TimeoutAfter(10)))
  }

  test("CREATE OR REPLACE DATABASE foo WAIT") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsReplace, NoOptions, IndefiniteWait))
  }

  test("CREATE OR REPLACE DATABASE foo NOWAIT") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsReplace, NoOptions, NoWait))
  }

  test("CREATE OR REPLACE DATABASE `_foo-bar42`") {
    yields(_ => ast.CreateDatabase(literal("_foo-bar42"), ast.IfExistsReplace, NoOptions, NoWait)(pos))
  }

  test("CREATE OR REPLACE DATABASE foo IF NOT EXISTS") {
    yields(ast.CreateDatabase(literalFoo, ast.IfExistsInvalidSyntax, NoOptions, NoWait))
  }

  test("CREATE DATABASE") {
    // missing db name but parses as 'normal' cypher CREATE...
    assertFailsWithMessage(
      testName,
      s"""Invalid input '': expected a parameter or an identifier (line 1, column 16 (offset: 15))"""
    )
  }

  test("CREATE DATABASE \"foo.bar\"") {
    failsToParse
  }

  test("CREATE DATABASE foo-bar42") {
    failsToParse
  }

  test("CREATE DATABASE _foo-bar42") {
    failsToParse
  }

  test("CREATE DATABASE 42foo-bar") {
    failsToParse
  }

  test("CREATE DATABASE _foo-bar42 IF NOT EXISTS") {
    failsToParse
  }

  test("CREATE DATABASE  IF NOT EXISTS") {
    val exceptionMessage =
      s"""Invalid input 'NOT': expected
         |  "."
         |  "IF"
         |  "NOWAIT"
         |  "OPTIONS"
         |  "WAIT"
         |  <EOF> (line 1, column 21 (offset: 20))""".stripMargin

    assertFailsWithMessage(testName, exceptionMessage)
  }

  test("CREATE DATABASE foo IF EXISTS") {
    failsToParse
  }

  test("CREATE DATABASE foo WAIT -12") {
    failsToParse
  }

  test("CREATE DATABASE foo WAIT 3.14") {
    assertFailsWithMessage(
      testName,
      "Invalid input '3.14': expected <EOF> or <UNSIGNED_DECIMAL_INTEGER> (line 1, column 26 (offset: 25))"
    )
  }

  test("CREATE DATABASE foo WAIT bar") {
    failsToParse
  }

  test("CREATE OR REPLACE DATABASE _foo-bar42") {
    failsToParse
  }

  test("CREATE OR REPLACE DATABASE") {
    assertFailsWithMessage(
      testName,
      s"""Invalid input '': expected a parameter or an identifier (line 1, column 27 (offset: 26))"""
    )
  }

  test(
    "CREATE DATABASE foo OPTIONS {existingData: 'use', existingDataSeedInstance: '84c3ee6f-260e-47db-a4b6-589c807f2c2e'}"
  ) {
    assertAst(
      CreateDatabase(
        Left("foo"),
        IfExistsThrowError,
        OptionsMap(Map(
          "existingData" -> StringLiteral("use")(1, 44, 43),
          "existingDataSeedInstance" -> StringLiteral("84c3ee6f-260e-47db-a4b6-589c807f2c2e")(1, 77, 76)
        )),
        NoWait
      )(defaultPos)
    )
  }

  test(
    "CREATE DATABASE foo OPTIONS {existingData: 'use', existingDataSeedInstance: '84c3ee6f-260e-47db-a4b6-589c807f2c2e'} WAIT"
  ) {
    assertAst(
      CreateDatabase(
        Left("foo"),
        IfExistsThrowError,
        OptionsMap(Map(
          "existingData" -> StringLiteral("use")(1, 44, 43),
          "existingDataSeedInstance" -> StringLiteral("84c3ee6f-260e-47db-a4b6-589c807f2c2e")(1, 77, 76)
        )),
        IndefiniteWait
      )(defaultPos)
    )
  }

  test("CREATE DATABASE foo OPTIONS $param") {
    assertAst(
      CreateDatabase(Left("foo"), IfExistsThrowError, OptionsParam(Parameter("param", CTMap)(1, 29, 28)), NoWait)(
        defaultPos
      )
    )
  }

  test("CREATE DATABASE alias") {
    yields(_ => ast.CreateDatabase(literal("alias"), ast.IfExistsThrowError, NoOptions, NoWait)(pos))
  }

  test("CREATE DATABASE alias IF NOT EXISTS") {
    yields(_ => ast.CreateDatabase(literal("alias"), ast.IfExistsDoNothing, NoOptions, NoWait)(pos))
  }

  // DROP DATABASE

  test("DROP DATABASE foo") {
    yields(ast.DropDatabase(literalFoo, ifExists = false, DestroyData, NoWait))
  }

  test("DROP DATABASE alias") {
    yields(ast.DropDatabase(literal("alias"), ifExists = false, DestroyData, NoWait))
  }

  test("DROP DATABASE alias WAIT") {
    yields(ast.DropDatabase(literal("alias"), ifExists = false, DestroyData, IndefiniteWait))
  }

  test("DROP DATABASE alias NOWAIT") {
    yields(ast.DropDatabase(literal("alias"), ifExists = false, DestroyData, NoWait))
  }

  test("DROP DATABASE $foo") {
    yields(ast.DropDatabase(paramFoo, ifExists = false, DestroyData, NoWait))
  }

  test("DROP DATABASE foo WAIT") {
    yields(ast.DropDatabase(literalFoo, ifExists = false, DestroyData, IndefiniteWait))
  }

  test("DROP DATABASE foo WAIT 10") {
    yields(ast.DropDatabase(literal("foo"), ifExists = false, DestroyData, TimeoutAfter(10)))
  }

  test("DROP DATABASE foo WAIT 10 SEC") {
    yields(ast.DropDatabase(literal("foo"), ifExists = false, DestroyData, TimeoutAfter(10)))
  }

  test("DROP DATABASE foo WAIT 10 SECOND") {
    yields(ast.DropDatabase(literal("foo"), ifExists = false, DestroyData, TimeoutAfter(10)))
  }

  test("DROP DATABASE foo WAIT 10 SECONDS") {
    yields(ast.DropDatabase(literal("foo"), ifExists = false, DestroyData, TimeoutAfter(10)))
  }

  test("DROP DATABASE foo NOWAIT") {
    yields(ast.DropDatabase(literalFoo, ifExists = false, DestroyData, NoWait))
  }

  test("DROP DATABASE `foo.bar`") {
    yields(_ => ast.DropDatabase(literalFooBar, ifExists = false, DestroyData, NoWait)(pos))
  }

  test("DROP DATABASE foo.bar") {
    yields(_ => ast.DropDatabase(literalFooBar, ifExists = false, DestroyData, NoWait)(pos))
  }

  test("DROP DATABASE foo IF EXISTS") {
    yields(ast.DropDatabase(literalFoo, ifExists = true, DestroyData, NoWait))
  }

  test("DROP DATABASE foo IF EXISTS WAIT") {
    yields(ast.DropDatabase(literalFoo, ifExists = true, DestroyData, IndefiniteWait))
  }

  test("DROP DATABASE foo IF EXISTS NOWAIT") {
    yields(ast.DropDatabase(literalFoo, ifExists = true, DestroyData, NoWait))
  }

  test("DROP DATABASE foo DUMP DATA") {
    yields(ast.DropDatabase(literalFoo, ifExists = false, DumpData, NoWait))
  }

  test("DROP DATABASE foo DESTROY DATA") {
    yields(ast.DropDatabase(literalFoo, ifExists = false, DestroyData, NoWait))
  }

  test("DROP DATABASE foo IF EXISTS DUMP DATA") {
    yields(ast.DropDatabase(literalFoo, ifExists = true, DumpData, NoWait))
  }

  test("DROP DATABASE foo IF EXISTS DESTROY DATA") {
    yields(ast.DropDatabase(literalFoo, ifExists = true, DestroyData, NoWait))
  }

  test("DROP DATABASE foo IF EXISTS DESTROY DATA WAIT") {
    yields(ast.DropDatabase(literal("foo"), ifExists = true, DestroyData, IndefiniteWait))
  }

  test("DROP DATABASE") {
    assertFailsWithMessage(
      testName,
      s"""Invalid input '': expected a parameter or an identifier (line 1, column 14 (offset: 13))"""
    )
  }

  test("DROP DATABASE  IF EXISTS") {
    failsToParse
  }

  test("DROP DATABASE foo IF NOT EXISTS") {
    failsToParse
  }

  test("DROP DATABASE KEEP DATA") {
    val exceptionMessage =
      s"""Invalid input 'DATA': expected
         |  "."
         |  "DESTROY"
         |  "DUMP"
         |  "IF"
         |  "NOWAIT"
         |  "WAIT"
         |  <EOF> (line 1, column 20 (offset: 19))""".stripMargin

    assertFailsWithMessage(testName, exceptionMessage)
  }

  // ALTER DATABASE
  Seq(
    ("READ ONLY", ReadOnlyAccess),
    ("READ WRITE", ReadWriteAccess)
  ).foreach {
    case (accessKeyword, accessType) =>
      test(s"ALTER DATABASE foo SET ACCESS $accessKeyword") {
        assertAst(AlterDatabase(literalFoo, ifExists = false, accessType)(defaultPos))
      }

      test(s"ALTER DATABASE $$foo SET ACCESS $accessKeyword") {
        assertAst(AlterDatabase(Right(expressions.Parameter("foo", CTString)(1, 16, 15)), ifExists = false, accessType)(
          defaultPos
        ))
      }

      test(s"ALTER DATABASE `foo.bar` SET ACCESS $accessKeyword") {
        assertAst(AlterDatabase(literalFooBar, ifExists = false, accessType)(defaultPos))
      }

      test(s"USE system ALTER DATABASE foo SET ACCESS $accessKeyword") {
        // can parse USE clause, but is not included in AST
        assertAst(AlterDatabase(literalFoo, ifExists = false, accessType)(1, 12, 11))
      }

      test(s"ALTER DATABASE foo IF EXISTS SET ACCESS $accessKeyword") {
        assertAst(AlterDatabase(literalFoo, ifExists = true, accessType)(defaultPos))
      }
  }

  test("ALTER DATABASE") {
    assertFailsWithMessage(
      testName,
      "Invalid input '': expected a parameter or an identifier (line 1, column 15 (offset: 14))"
    )
  }

  test("ALTER DATABASE foo") {
    assertFailsWithMessage(
      testName,
      "Invalid input '': expected \".\", \"IF\" or \"SET\" (line 1, column 19 (offset: 18))"
    )
  }

  test("ALTER DATABASE foo SET READ ONLY") {
    assertFailsWithMessage(testName, "Invalid input 'READ': expected \"ACCESS\" (line 1, column 24 (offset: 23))")
  }

  test("ALTER DATABASE foo ACCESS READ WRITE") {
    assertFailsWithMessage(
      testName,
      "Invalid input 'ACCESS': expected \".\", \"IF\" or \"SET\" (line 1, column 20 (offset: 19))"
    )
  }

  test("ALTER DATABASE foo SET ACCESS READ") {
    assertFailsWithMessage(
      testName,
      "Invalid input '': expected \"ONLY\" or \"WRITE\" (line 1, column 35 (offset: 34))"
    )
  }

  test("ALTER DATABASE foo SET ACCESS READWRITE'") {
    assertFailsWithMessage(testName, "Invalid input 'READWRITE': expected \"READ\" (line 1, column 31 (offset: 30))")
  }

  test("ALTER DATABASE foo SET ACCESS READ_ONLY") {
    assertFailsWithMessage(testName, "Invalid input 'READ_ONLY': expected \"READ\" (line 1, column 31 (offset: 30))")
  }

  test("ALTER DATABASE foo SET ACCESS WRITE") {
    assertFailsWithMessage(testName, "Invalid input 'WRITE': expected \"READ\" (line 1, column 31 (offset: 30))")
  }

  // Set ACCESS multiple times in the same command
  test("ALTER DATABASE foo SET ACCESS READ ONLY SET ACCESS READ WRITE") {
    assertFailsWithMessage(testName, "Invalid input 'SET': expected <EOF> (line 1, column 41 (offset: 40))")
  }

  // Wrong order between IF EXISTS and SET
  test("ALTER DATABASE foo SET ACCESS READ ONLY IF EXISTS") {
    assertFailsWithMessage(testName, "Invalid input 'IF': expected <EOF> (line 1, column 41 (offset: 40))")
  }

  // IF NOT EXISTS instead of IF EXISTS
  test("ALTER DATABASE foo IF NOT EXISTS SET ACCESS READ ONLY") {
    assertFailsWithMessage(testName, "Invalid input 'NOT': expected \"EXISTS\" (line 1, column 23 (offset: 22))")
  }

  // ALTER with OPTIONS
  test("ALTER DATABASE foo SET ACCESS READ WRITE OPTIONS {existingData: 'use'}") {
    assertFailsWithMessage(testName, "Invalid input 'OPTIONS': expected <EOF> (line 1, column 42 (offset: 41))")
  }

  // ALTER OR REPLACE
  test("ALTER OR REPLACE DATABASE foo SET ACCESS READ WRITE") {
    assertFailsWithMessage(
      testName,
      "Invalid input 'OR': expected \"ALIAS\", \"CURRENT\", \"DATABASE\" or \"USER\" (line 1, column 7 (offset: 6))"
    )
  }

  // START DATABASE

  test("START DATABASE foo") {
    yields(ast.StartDatabase(literalFoo, NoWait))
  }

  test("START DATABASE $foo") {
    yields(ast.StartDatabase(paramFoo, NoWait))
  }

  test("START DATABASE foo WAIT") {
    yields(ast.StartDatabase(literalFoo, IndefiniteWait))
  }

  test("START DATABASE foo WAIT 5") {
    yields(ast.StartDatabase(literal("foo"), TimeoutAfter(5)))
  }

  test("START DATABASE foo WAIT 5 SEC") {
    yields(ast.StartDatabase(literal("foo"), TimeoutAfter(5)))
  }

  test("START DATABASE foo WAIT 5 SECOND") {
    yields(ast.StartDatabase(literal("foo"), TimeoutAfter(5)))
  }

  test("START DATABASE foo WAIT 5 SECONDS") {
    yields(ast.StartDatabase(literal("foo"), TimeoutAfter(5)))
  }

  test("START DATABASE foo NOWAIT") {
    yields(ast.StartDatabase(literalFoo, NoWait))
  }

  test("START DATABASE `foo.bar`") {
    yields(_ => ast.StartDatabase(literalFooBar, NoWait)(pos))
  }

  test("START DATABASE foo.bar") {
    yields(_ => ast.StartDatabase(literalFooBar, NoWait)(pos))
  }

  test("START DATABASE") {
    assertFailsWithMessage(
      testName,
      "Invalid input '': expected a parameter or an identifier (line 1, column 15 (offset: 14))"
    )
  }

  // STOP DATABASE

  test("STOP DATABASE foo") {
    yields(ast.StopDatabase(literalFoo, NoWait))
  }

  test("STOP DATABASE $foo") {
    yields(ast.StopDatabase(paramFoo, NoWait))
  }

  test("STOP DATABASE foo WAIT") {
    yields(ast.StopDatabase(literalFoo, IndefiniteWait))
  }

  test("STOP DATABASE foo WAIT 99") {
    yields(ast.StopDatabase(literal("foo"), TimeoutAfter(99)))
  }

  test("STOP DATABASE foo WAIT 99 SEC") {
    yields(ast.StopDatabase(literal("foo"), TimeoutAfter(99)))
  }

  test("STOP DATABASE foo WAIT 99 SECOND") {
    yields(ast.StopDatabase(literal("foo"), TimeoutAfter(99)))
  }

  test("STOP DATABASE foo WAIT 99 SECONDS") {
    yields(ast.StopDatabase(literal("foo"), TimeoutAfter(99)))
  }

  test("STOP DATABASE foo NOWAIT") {
    yields(ast.StopDatabase(literalFoo, NoWait))
  }

  test("STOP DATABASE `foo.bar`") {
    yields(_ => ast.StopDatabase(literalFooBar, NoWait)(pos))
  }

  test("STOP DATABASE foo.bar") {
    yields(_ => ast.StopDatabase(literalFooBar, NoWait)(pos))
  }

  test("STOP DATABASE") {
    assertFailsWithMessage(
      testName,
      "Invalid input '': expected a parameter or an identifier (line 1, column 14 (offset: 13))"
    )
  }
}
