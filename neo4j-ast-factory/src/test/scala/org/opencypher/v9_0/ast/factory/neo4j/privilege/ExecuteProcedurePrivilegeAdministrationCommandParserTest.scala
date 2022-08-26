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
          Seq[Immutable](true, false).foreach {
            immutable =>
              val immutableString = immutableOrEmpty(immutable)
              test(s"$verb$immutableString $execute * ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("*")), Seq(literalRole), immutable))
              }

              // The following two tests check that the plural form EXECUTE [BOOSTED] PROCEDURES is valid

              test(s"$verb$immutableString ${execute}S * ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("*")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString ${execute}S `*` ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("*")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute apoc.procedure ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("apoc.procedure")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString ${execute}S apoc.procedure ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("apoc.procedure")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute apoc.math.sin ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("apoc.math.sin")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute apoc* ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("apoc*")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute *apoc ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("*apoc")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute *apoc, *.sin ON DBMS $preposition role") {
                yields(func(
                  action,
                  List(procedureQualifier("*apoc"), procedureQualifier("*.sin")),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $execute *.sin, apoc* ON DBMS $preposition role") {
                yields(func(
                  action,
                  List(procedureQualifier("*.sin"), procedureQualifier("apoc*")),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $execute *.sin ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("*.sin")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute apoc.*.math.* ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("apoc.*.math.*")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute math.*n ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("math.*n")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute math.si? ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("math.si?")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute mat*.sin ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("mat*.sin")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute mat?.sin ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("mat?.sin")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute ?ath.sin ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("?ath.sin")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute mat?.`a.\n`.*n ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("mat?.a.\n.*n")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute `mat?`.`a.\n`.`*n` ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("mat?.a.\n.*n")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute `a b` ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("a b")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute a b ON DBMS $preposition role") {
                assertAst(
                  func(action, List(ProcedureQualifier("ab")(defaultPos)), Seq(Left("role")), immutable)(defaultPos)
                )
              }

              test(s"$verb$immutableString $execute apoc.math.* ON DBMS $preposition role") {
                yields(func(action, List(procedureQualifier("apoc.math.*")), Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $execute math.sin, math.cos ON DBMS $preposition role") {
                yields(func(
                  action,
                  List(procedureQualifier("math.sin"), procedureQualifier("math.cos")),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $execute apoc.math.sin, math.* ON DBMS $preposition role") {
                yields(func(
                  action,
                  List(procedureQualifier("apoc.math.sin"), procedureQualifier("math.*")),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $execute * $preposition role") {
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

              test(s"$verb$immutableString $execute * ON DATABASE * $preposition role") {
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

              test(s"$verb$immutableString $execute `ab?`* ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $execute ".length
                assertFailsWithMessage(
                  testName,
                  s"""Invalid input 'ab?': expected "*", ".", "?" or an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
                )
              }

              test(s"$verb$immutableString $execute a`ab?` ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $execute a".length
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

              test(s"$verb$immutableString $execute ab?`%ab`* ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $execute ab?".length
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

              test(s"$verb$immutableString $execute apoc.`*`ab? ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $execute apoc.".length
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

              test(s"$verb$immutableString $execute apoc.*`ab?` ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $execute apoc.*".length
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

              test(s"$verb$immutableString $execute `ap`oc.ab? ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $execute ".length
                assertFailsWithMessage(
                  testName,
                  s"""Invalid input 'ap': expected "*", ".", "?" or an identifier (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
                )
              }

              test(s"$verb$immutableString $execute ap`oc`.ab? ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $execute ap".length
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
  }

  Seq(
    ("GRANT", "TO", grantDbmsPrivilege: dbmsPrivilegeFunc),
    ("DENY", "TO", denyDbmsPrivilege: dbmsPrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantDbmsPrivilege: dbmsPrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyDbmsPrivilege: dbmsPrivilegeFunc),
    ("REVOKE", "FROM", revokeDbmsPrivilege: dbmsPrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: dbmsPrivilegeFunc) =>
      Seq[Immutable](true, false).foreach {
        immutable =>
          val immutableString = immutableOrEmpty(immutable)
          Seq(
            "EXECUTE ADMIN PROCEDURES",
            "EXECUTE ADMINISTRATOR PROCEDURES"
          ).foreach {
            command =>
              test(s"$verb$immutableString $command ON DBMS $preposition role") {
                yields(func(ExecuteAdminProcedureAction, Seq(literalRole), immutable))
              }

              test(s"$verb$immutableString $command * ON DBMS $preposition role") {
                val offset = s"$verb$immutableString $command ".length
                assertFailsWithMessage(
                  testName,
                  s"""Invalid input '*': expected "ON" (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
                )
              }

              test(s"$verb$immutableString $command ON DATABASE * $preposition role") {
                val offset = s"$verb$immutableString $command ON ".length
                assertFailsWithMessage(
                  testName,
                  s"""Invalid input 'DATABASE': expected "DBMS" (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
                )
              }

          }

          test(s"$verb$immutableString EXECUTE ADMIN PROCEDURE ON DBMS $preposition role") {
            val offset = s"$verb$immutableString EXECUTE ADMIN ".length
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'PROCEDURE': expected "PROCEDURES" (line 1, column ${offset + 1} (offset: $offset))""".stripMargin
            )
          }
      }
  }

  private def procedureQualifier(procName: String): InputPosition => ProcedureQualifier =
    ProcedureQualifier(procName)(_)
}
