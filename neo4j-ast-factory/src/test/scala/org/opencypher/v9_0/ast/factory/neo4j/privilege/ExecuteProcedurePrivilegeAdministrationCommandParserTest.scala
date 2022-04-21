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
package org.opencypher.v9_0.ast.factory.neo4j.privilege

import org.opencypher.v9_0.ast.ExecuteAdminProcedureAction
import org.opencypher.v9_0.ast.ExecuteBoostedProcedureAction
import org.opencypher.v9_0.ast.ExecuteProcedureAction
import org.opencypher.v9_0.ast.ProcedureQualifier
import org.opencypher.v9_0.ast.factory.neo4j.AdministrationAndSchemaCommandParserTestBase
import org.opencypher.v9_0.util.InputPosition

class ExecuteProcedurePrivilegeAdministrationCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {

  Seq(
    ("GRANT", "TO", grantExecuteProcedurePrivilege: executeProcedurePrivilegeFunc),
    ("DENY", "TO", denyExecuteProcedurePrivilege: executeProcedurePrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantExecuteProcedurePrivilege: executeProcedurePrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyExecuteProcedurePrivilege: executeProcedurePrivilegeFunc),
    ("REVOKE", "FROM", revokeExecuteProcedurePrivilege: executeProcedurePrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: executeProcedurePrivilegeFunc) =>
      Seq(
        ("EXECUTE PROCEDURE", ExecuteProcedureAction),
        ("EXECUTE BOOSTED PROCEDURE", ExecuteBoostedProcedureAction)
      ).foreach {
        case (execute, action) =>
          test(s"$verb $execute * ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("*")), Seq(literalRole)))
          }

          // The following two tests check that the plural form EXECUTE [BOOSTED] PROCEDURES is valid

          test(s"$verb ${execute}S * ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("*")), Seq(literalRole)))
          }

          test(s"$verb ${execute}S `*` ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("*")), Seq(literalRole)))
          }

          test(s"$verb $execute apoc.procedure ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("apoc.procedure")), Seq(literalRole)))
          }

          test(s"$verb ${execute}S apoc.procedure ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("apoc.procedure")), Seq(literalRole)))
          }

          test(s"$verb $execute apoc.math.sin ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("apoc.math.sin")), Seq(literalRole)))
          }

          test(s"$verb $execute apoc* ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("apoc*")), Seq(literalRole)))
          }

          test(s"$verb $execute *apoc ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("*apoc")), Seq(literalRole)))
          }

          test(s"$verb $execute *apoc, *.sin ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("*apoc"), procedureQualifier("*.sin")), Seq(literalRole)))
          }

          test(s"$verb $execute *.sin, apoc* ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("*.sin"), procedureQualifier("apoc*")), Seq(literalRole)))
          }

          test(s"$verb $execute *.sin ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("*.sin")), Seq(literalRole)))
          }

          test(s"$verb $execute apoc.*.math.* ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("apoc.*.math.*")), Seq(literalRole)))
          }

          test(s"$verb $execute math.*n ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("math.*n")), Seq(literalRole)))
          }

          test(s"$verb $execute math.si? ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("math.si?")), Seq(literalRole)))
          }

          test(s"$verb $execute mat*.sin ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("mat*.sin")), Seq(literalRole)))
          }

          test(s"$verb $execute mat?.sin ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("mat?.sin")), Seq(literalRole)))
          }

          test(s"$verb $execute ?ath.sin ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("?ath.sin")), Seq(literalRole)))
          }

          test(s"$verb $execute mat?.`a.\n`.*n ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("mat?.a.\n.*n")), Seq(literalRole)))
          }

          test(s"$verb $execute `mat?`.`a.\n`.`*n` ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("mat?.a.\n.*n")), Seq(literalRole)))
          }

          test(s"$verb $execute `a b` ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("a b")), Seq(literalRole)))
          }

          test(s"$verb $execute a b ON DBMS $preposition role") {
            assertAst(func(action, List(ProcedureQualifier("ab")(defaultPos)), Seq(Left("role")))(defaultPos))
          }

          test(s"$verb $execute apoc.math.* ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("apoc.math.*")), Seq(literalRole)))
          }

          test(s"$verb $execute math.sin, math.cos ON DBMS $preposition role") {
            yields(func(action, List(procedureQualifier("math.sin"), procedureQualifier("math.cos")), Seq(literalRole)))
          }

          test(s"$verb $execute apoc.math.sin, math.* ON DBMS $preposition role") {
            yields(func(
              action,
              List(procedureQualifier("apoc.math.sin"), procedureQualifier("math.*")),
              Seq(literalRole)
            ))
          }

          test(s"$verb $execute * $preposition role") {
            val offset = testName.length
            assertFailsWithMessage(
              testName,
              s"""Invalid input '': expected
                 |  "*"
                 |  "."
                 |  "?"
                 |  "ON"
                 |  an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $execute * ON DATABASE * $preposition role") {
            val offset = testName.length
            assertFailsWithMessage(
              testName,
              s"""Invalid input '': expected
                 |  "*"
                 |  "."
                 |  "?"
                 |  "ON"
                 |  an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          // Tests for invalid escaping

          test(s"$verb $execute `ab?`* ON DBMS $preposition role") {
            val offset = s"$verb $execute ".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'ab?': expected "*", ".", "?" or an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $execute a`ab?` ON DBMS $preposition role") {
            val offset = s"$verb $execute a".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'ab?': expected
                 |  "*"
                 |  "."
                 |  "?"
                 |  "ON"
                 |  an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $execute ab?`%ab`* ON DBMS $preposition role") {
            val offset = s"$verb $execute ab?".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input '%ab': expected
                 |  "*"
                 |  "."
                 |  "?"
                 |  "ON"
                 |  "YIELD"
                 |  an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $execute apoc.`*`ab? ON DBMS $preposition role") {
            val offset = s"$verb $execute apoc.".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input '*': expected
                 |  "*"
                 |  "."
                 |  "?"
                 |  "YIELD"
                 |  an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $execute apoc.*`ab?` ON DBMS $preposition role") {
            val offset = s"$verb $execute apoc.*".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'ab?': expected
                 |  "*"
                 |  "."
                 |  "?"
                 |  "ON"
                 |  an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $execute `ap`oc.ab? ON DBMS $preposition role") {
            val offset = s"$verb $execute ".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'ap': expected "*", ".", "?" or an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $execute ap`oc`.ab? ON DBMS $preposition role") {
            val offset = s"$verb $execute ap".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'oc': expected
                 |  "*"
                 |  "."
                 |  "?"
                 |  "ON"
                 |  an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }
      }
  }

  Seq(
    ("GRANT", "TO", grantDbmsPrivilege: dbmsPrivilegeFunc),
    ("DENY", "TO", denyDbmsPrivilege: dbmsPrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantDbmsPrivilege: dbmsPrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyDbmsPrivilege: dbmsPrivilegeFunc),
    ("REVOKE", "FROM", revokeDbmsPrivilege: dbmsPrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: dbmsPrivilegeFunc) =>
      Seq(
        "EXECUTE ADMIN PROCEDURES",
        "EXECUTE ADMINISTRATOR PROCEDURES"
      ).foreach {
        command =>
          test(s"$verb $command ON DBMS $preposition role") {
            yields(func(ExecuteAdminProcedureAction, Seq(literalRole)))
          }

          test(s"$verb $command * ON DBMS $preposition role") {
            val offset = s"$verb $command ".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input '*': expected "ON" (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

          test(s"$verb $command ON DATABASE * $preposition role") {
            val offset = s"$verb $command ON ".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'DATABASE': expected "DBMS" (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }

      }

      test(s"$verb EXECUTE ADMIN PROCEDURE ON DBMS $preposition role") {
        val offset = s"$verb EXECUTE ADMIN ".length
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'PROCEDURE': expected "PROCEDURES" (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
        )
      }
  }

  private def procedureQualifier(procName: String): InputPosition => ProcedureQualifier =
    ProcedureQualifier(procName)(_)
}
