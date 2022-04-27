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

import org.opencypher.v9_0.ast
import org.opencypher.v9_0.ast.factory.neo4j.AdministrationAndSchemaCommandParserTestBase

class ShowPrivilegesAdministrationCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {

  // Show privileges

  test("SHOW PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowAllPrivileges()(pos), None))
  }

  test("SHOW PRIVILEGE") {
    yields(ast.ShowPrivileges(ast.ShowAllPrivileges()(pos), None))
  }

  test("use system show privileges") {
    yields(ast.ShowPrivileges(ast.ShowAllPrivileges()(pos), None))
  }

  test("SHOW ALL PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowAllPrivileges()(pos), None))
  }

  // Show user privileges

  test("SHOW USER user PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literalUser))(pos), None))
  }

  test("SHOW USERS $user PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(paramUser))(pos), None))
  }

  test("SHOW USER `us%er` PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literal("us%er")))(pos), None))
  }

  test("SHOW USER user, $user PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literalUser, paramUser))(pos), None))
  }

  test("SHOW USER user, $user PRIVILEGE") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literalUser, paramUser))(pos), None))
  }

  test("SHOW USERS user1, $user, user2 PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literalUser1, paramUser, literal("user2")))(pos), None))
  }

  test("SHOW USER PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUserPrivileges(None)(pos), None))
  }

  test("SHOW USERS PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUserPrivileges(None)(pos), None))
  }

  test("SHOW USER PRIVILEGE") {
    yields(ast.ShowPrivileges(ast.ShowUserPrivileges(None)(pos), None))
  }

  test("SHOW USER privilege PRIVILEGE") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literal("privilege")))(pos), None))
  }

  test("SHOW USER privilege, privileges PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literal("privilege"), literal("privileges")))(pos), None))
  }

  test(s"SHOW USER defined PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literal("defined")))(pos), None))
  }

  test(s"SHOW USERS yield, where PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowUsersPrivileges(List(literal("yield"), literal("where")))(pos), None))
  }

  // Show role privileges

  test("SHOW ROLE role PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(literalRole))(pos), None))
  }

  test("SHOW ROLE role PRIVILEGE") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(literalRole))(pos), None))
  }

  test("SHOW ROLE $role PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(paramRole))(pos), None))
  }

  test("SHOW ROLES `ro%le` PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(literal("ro%le")))(pos), None))
  }

  test("SHOW ROLE role1, $roleParam, role2, role3 PRIVILEGES") {
    yields(ast.ShowPrivileges(
      ast.ShowRolesPrivileges(List(literalRole1, param("roleParam"), literalRole2, literal("role3")))(pos),
      None
    ))
  }

  test("SHOW ROLES role1, $roleParam1, role2, $roleParam2 PRIVILEGES") {
    yields(ast.ShowPrivileges(
      ast.ShowRolesPrivileges(List(literalRole1, param("roleParam1"), literalRole2, param("roleParam2")))(pos),
      None
    ))
  }

  test("SHOW ROLES privilege PRIVILEGE") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(literal("privilege")))(pos), None))
  }

  test("SHOW ROLE privilege, privileges PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(literal("privilege"), literal("privileges")))(pos), None))
  }

  test(s"SHOW ROLES yield, where PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(literal("yield"), literal("where")))(pos), None))
  }

  test(s"SHOW ROLES with PRIVILEGES") {
    yields(ast.ShowPrivileges(ast.ShowRolesPrivileges(List(literal("with")))(pos), None))
  }

  // Show privileges as commands

  test("SHOW PRIVILEGES AS COMMAND") {
    yields(ast.ShowPrivilegeCommands(ast.ShowAllPrivileges()(pos), asRevoke = false, None))
  }

  test("SHOW PRIVILEGES AS COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowAllPrivileges()(pos), asRevoke = false, None))
  }

  test("SHOW PRIVILEGES AS REVOKE COMMAND") {
    yields(ast.ShowPrivilegeCommands(ast.ShowAllPrivileges()(pos), asRevoke = true, None))
  }

  test("SHOW PRIVILEGES AS REVOKE COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowAllPrivileges()(pos), asRevoke = true, None))
  }

  test("SHOW ALL PRIVILEGES AS COMMAND") {
    yields(ast.ShowPrivilegeCommands(ast.ShowAllPrivileges()(pos), asRevoke = false, None))
  }

  test("SHOW ALL PRIVILEGE AS COMMAND") {
    yields(ast.ShowPrivilegeCommands(ast.ShowAllPrivileges()(pos), asRevoke = false, None))
  }

  test("SHOW ALL PRIVILEGES AS REVOKE COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowAllPrivileges()(pos), asRevoke = true, None))
  }

  test("SHOW USER user PRIVILEGES AS COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUsersPrivileges(List(literalUser))(pos), asRevoke = false, None))
  }

  test("SHOW USERS $user PRIVILEGES AS REVOKE COMMAND") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUsersPrivileges(List(paramUser))(pos), asRevoke = true, None))
  }

  test("SHOW USER `us%er` PRIVILEGES AS COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUsersPrivileges(List(literal("us%er")))(pos), asRevoke = false, None))
  }

  test("SHOW USER `us%er` PRIVILEGE AS COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUsersPrivileges(List(literal("us%er")))(pos), asRevoke = false, None))
  }

  test("SHOW USER user, $user PRIVILEGES AS REVOKE COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUsersPrivileges(List(literalUser, paramUser))(pos), asRevoke = true, None))
  }

  test("SHOW USER PRIVILEGES AS COMMAND") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUserPrivileges(None)(pos), asRevoke = false, None))
  }

  test("SHOW USERS PRIVILEGES AS REVOKE COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUserPrivileges(None)(pos), asRevoke = true, None))
  }

  test("SHOW USERS PRIVILEGE AS REVOKE COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowUserPrivileges(None)(pos), asRevoke = true, None))
  }

  test("SHOW ROLE role PRIVILEGES AS COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowRolesPrivileges(List(literalRole))(pos), asRevoke = false, None))
  }

  test("SHOW ROLE role PRIVILEGE AS COMMANDS") {
    yields(ast.ShowPrivilegeCommands(ast.ShowRolesPrivileges(List(literalRole))(pos), asRevoke = false, None))
  }

  test("SHOW ROLE $role PRIVILEGES AS REVOKE COMMAND") {
    yields(ast.ShowPrivilegeCommands(ast.ShowRolesPrivileges(List(paramRole))(pos), asRevoke = true, None))
  }

  // yield / skip / limit / order by / where

  Seq(
    (" AS COMMANDS", false),
    (" AS REVOKE COMMANDS", true),
    ("", false)
  ).foreach { case (optionalAsRev: String, asRev) =>
    Seq(
      ("", ast.ShowAllPrivileges()(pos)),
      ("ALL", ast.ShowAllPrivileges()(pos)),
      ("USER", ast.ShowUserPrivileges(None)(pos)),
      ("USER neo4j", ast.ShowUsersPrivileges(List(literal("neo4j")))(pos)),
      ("USERS neo4j, $user", ast.ShowUsersPrivileges(List(literal("neo4j"), paramUser))(pos)),
      ("ROLES $role", ast.ShowRolesPrivileges(List(paramRole))(pos)),
      ("ROLE $role, reader", ast.ShowRolesPrivileges(List(paramRole, literal("reader")))(pos))
    ).foreach { case (privType, privilege) =>
      Seq(
        "PRIVILEGE",
        "PRIVILEGES"
      ).foreach { privilegeOrPrivileges =>
        test(s"SHOW $privType $privilegeOrPrivileges$optionalAsRev WHERE access = 'GRANTED'") {
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(privilege, Some(Right(where(equals(accessVar, grantedString))))))
          } else {
            yields(ast.ShowPrivilegeCommands(privilege, asRev, Some(Right(where(equals(accessVar, grantedString))))))
          }
        }

        test(s"SHOW $privType $privilegeOrPrivileges$optionalAsRev WHERE access = 'GRANTED' AND action = 'match'") {
          val accessPredicate = equals(accessVar, grantedString)
          val matchPredicate = equals(varFor(actionString), literalString("match"))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(privilege, Some(Right(where(and(accessPredicate, matchPredicate))))))
          } else {
            yields(ast.ShowPrivilegeCommands(
              privilege,
              asRev,
              Some(Right(where(and(accessPredicate, matchPredicate))))
            ))
          }
        }

        test(s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD access ORDER BY access") {
          val orderByClause = orderBy(sortItem(accessVar))
          val columns = yieldClause(returnItems(variableReturnItem(accessString)), Some(orderByClause))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(privilege, Some(Left((columns, None)))))
          } else {
            yields(ast.ShowPrivilegeCommands(privilege, asRev, Some(Left((columns, None)))))
          }
        }

        test(
          s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD access ORDER BY access WHERE access ='none'"
        ) {
          val orderByClause = orderBy(sortItem(accessVar))
          val whereClause = where(equals(accessVar, noneString))
          val columns =
            yieldClause(returnItems(variableReturnItem(accessString)), Some(orderByClause), where = Some(whereClause))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(privilege, Some(Left((columns, None)))))
          } else {
            yields(ast.ShowPrivilegeCommands(privilege, asRev, Some(Left((columns, None)))))
          }
        }

        test(
          s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD access ORDER BY access SKIP 1 LIMIT 10 WHERE access ='none'"
        ) {
          val orderByClause = orderBy(sortItem(accessVar))
          val whereClause = where(equals(accessVar, noneString))
          val columns = yieldClause(
            returnItems(variableReturnItem(accessString)),
            Some(orderByClause),
            Some(skip(1)),
            Some(limit(10)),
            Some(whereClause)
          )
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(privilege, Some(Left((columns, None)))))
          } else {
            yields(ast.ShowPrivilegeCommands(privilege, asRev, Some(Left((columns, None)))))
          }
        }

        test(s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD access SKIP -1") {
          val columns = yieldClause(returnItems(variableReturnItem(accessString)), skip = Some(skip(-1)))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(privilege, Some(Left((columns, None)))))
          } else {
            yields(ast.ShowPrivilegeCommands(privilege, asRev, Some(Left((columns, None)))))
          }
        }

        test(
          s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD access, action RETURN access, count(action) ORDER BY access"
        ) {
          val orderByClause = orderBy(sortItem(accessVar))
          val accessColumn = variableReturnItem(accessString)
          val actionColumn = variableReturnItem(actionString)
          val countColumn = returnItem(count(varFor(actionString)), "count(action)")
          val yieldColumns = yieldClause(returnItems(accessColumn, actionColumn))
          val returns = returnClause(returnItems(accessColumn, countColumn), Some(orderByClause))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(privilege, Some(Left((yieldColumns, Some(returns))))))
          } else {
            yields(ast.ShowPrivilegeCommands(privilege, asRev, Some(Left((yieldColumns, Some(returns))))))
          }
        }

        test(
          s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD access, action SKIP 1 RETURN access, action"
        ) {
          val returnItemsPart = returnItems(variableReturnItem(accessString), variableReturnItem(actionString))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              privilege,
              Some(Left((yieldClause(returnItemsPart, skip = Some(skip(1))), Some(returnClause(returnItemsPart)))))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              privilege,
              asRev,
              Some(Left((yieldClause(returnItemsPart, skip = Some(skip(1))), Some(returnClause(returnItemsPart)))))
            ))
          }
        }

        test(
          s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD access, action WHERE access = 'none' RETURN action"
        ) {
          val accessColumn = variableReturnItem(accessString)
          val actionColumn = variableReturnItem(actionString)
          val whereClause = where(equals(accessVar, noneString))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              privilege,
              Some(Left((
                yieldClause(returnItems(accessColumn, actionColumn), where = Some(whereClause)),
                Some(returnClause(returnItems(actionColumn)))
              )))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              privilege,
              asRev,
              Some(Left((
                yieldClause(returnItems(accessColumn, actionColumn), where = Some(whereClause)),
                Some(returnClause(returnItems(actionColumn)))
              )))
            ))
          }
        }

        test(s"SHOW $privType $privilegeOrPrivileges$optionalAsRev YIELD * RETURN *") {
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              privilege,
              Some(Left((yieldClause(returnAllItems), Some(returnClause(returnAllItems)))))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              privilege,
              asRev,
              Some(Left((yieldClause(returnAllItems), Some(returnClause(returnAllItems)))))
            ))
          }
        }
      }
    }

    // yield and where edge cases

    type privilegeFunc = List[String] => ast.ShowPrivilegeScope

    def userPrivilegeFunc(users: List[String]): ast.ShowPrivilegeScope = {
      val literalUsers = users.map(u => literal(u))
      ast.ShowUsersPrivileges(literalUsers)(pos)
    }

    def rolePrivilegeFunc(roles: List[String]): ast.ShowPrivilegeScope = {
      val literalRoles = roles.map(r => literal(r))
      ast.ShowRolesPrivileges(literalRoles)(pos)
    }

    Seq(
      ("USER", userPrivilegeFunc: privilegeFunc),
      ("USERS", userPrivilegeFunc: privilegeFunc),
      ("ROLE", rolePrivilegeFunc: privilegeFunc),
      ("ROLES", rolePrivilegeFunc: privilegeFunc)
    ).foreach {
      case (privType: String, func: privilegeFunc) =>
        test(s"SHOW $privType yield PRIVILEGES$optionalAsRev YIELD access RETURN *") {
          val accessColumn = returnItems(variableReturnItem(accessString))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              func(List("yield")),
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              func(List("yield")),
              asRev,
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          }
        }

        test(s"SHOW $privType yield, where PRIVILEGES$optionalAsRev YIELD access RETURN *") {
          val accessColumn = returnItems(variableReturnItem(accessString))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              func(List("yield", "where")),
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              func(List("yield", "where")),
              asRev,
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          }
        }

        test(s"SHOW $privType where PRIVILEGE$optionalAsRev WHERE access = 'none'") {
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(func(List("where")), Some(Right(where(equals(accessVar, noneString))))))
          } else {
            yields(ast.ShowPrivilegeCommands(
              func(List("where")),
              asRev,
              Some(Right(where(equals(accessVar, noneString))))
            ))
          }
        }

        test(s"SHOW $privType privilege PRIVILEGE$optionalAsRev YIELD access RETURN *") {
          val accessColumn = returnItems(variableReturnItem(accessString))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              func(List("privilege")),
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              func(List("privilege")),
              asRev,
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          }
        }

        test(s"SHOW $privType privileges PRIVILEGES$optionalAsRev YIELD access RETURN *") {
          val accessColumn = returnItems(variableReturnItem(accessString))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              func(List("privileges")),
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              func(List("privileges")),
              asRev,
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          }
        }

        test(s"SHOW $privType privilege, privileges PRIVILEGES$optionalAsRev YIELD access RETURN *") {
          val accessColumn = returnItems(variableReturnItem(accessString))
          if (optionalAsRev.isEmpty) {
            yields(ast.ShowPrivileges(
              func(List("privilege", "privileges")),
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          } else {
            yields(ast.ShowPrivilegeCommands(
              func(List("privilege", "privileges")),
              asRev,
              Some(Left((yieldClause(accessColumn), Some(returnClause(returnAllItems)))))
            ))
          }
        }
    }
  }

  // Fails to parse

  test("SHOW PRIVILAGES") {
    val exceptionMessage =
      s"""Invalid input 'PRIVILAGES': expected
         |  "ALL"
         |  "BTREE"
         |  "BUILT"
         |  "CONSTRAINT"
         |  "CONSTRAINTS"
         |  "CURRENT"
         |  "DATABASE"
         |  "DATABASES"
         |  "DEFAULT"
         |  "EXIST"
         |  "EXISTENCE"
         |  "EXISTS"
         |  "FULLTEXT"
         |  "FUNCTION"
         |  "FUNCTIONS"
         |  "HOME"
         |  "INDEX"
         |  "INDEXES"
         |  "LOOKUP"
         |  "NODE"
         |  "POINT"
         |  "POPULATED"
         |  "PRIVILEGE"
         |  "PRIVILEGES"
         |  "PROCEDURE"
         |  "PROCEDURES"
         |  "PROPERTY"
         |  "RANGE"
         |  "REL"
         |  "RELATIONSHIP"
         |  "ROLE"
         |  "ROLES"
         |  "TEXT"
         |  "TRANSACTION"
         |  "TRANSACTIONS"
         |  "UNIQUE"
         |  "USER"
         |  "USERS" (line 1, column 6 (offset: 5))""".stripMargin

    assertFailsWithMessage(testName, exceptionMessage)
  }

  test("SHOW PRIVELAGES") {
    failsToParse
  }

  test("SHOW privalages") {
    failsToParse
  }

  test("SHOW ALL USER user PRIVILEGES") {
    val exceptionMessage =
      s"""Invalid input 'USER': expected
         |  "CONSTRAINT"
         |  "CONSTRAINTS"
         |  "FUNCTION"
         |  "FUNCTIONS"
         |  "INDEX"
         |  "INDEXES"
         |  "PRIVILEGE"
         |  "PRIVILEGES"
         |  "ROLES" (line 1, column 10 (offset: 9))""".stripMargin

    assertFailsWithMessage(testName, exceptionMessage)
  }

  test("SHOW USER us%er PRIVILEGES") {
    assertFailsWithMessage(
      testName,
      """Invalid input '%': expected ",", "PRIVILEGE" or "PRIVILEGES" (line 1, column 13 (offset: 12))"""
    )
  }

  test("SHOW ROLE PRIVILEGES") {
    assertFailsWithMessage(
      testName,
      """Invalid input '': expected ",", "PRIVILEGE" or "PRIVILEGES" (line 1, column 21 (offset: 20))"""
    )
  }

  test("SHOW ALL ROLE role PRIVILEGES") {
    val exceptionMessage =
      s"""Invalid input 'ROLE': expected
         |  "CONSTRAINT"
         |  "CONSTRAINTS"
         |  "FUNCTION"
         |  "FUNCTIONS"
         |  "INDEX"
         |  "INDEXES"
         |  "PRIVILEGE"
         |  "PRIVILEGES"
         |  "ROLES" (line 1, column 10 (offset: 9))""".stripMargin

    assertFailsWithMessage(testName, exceptionMessage)
  }

  test("SHOW ROLE ro%le PRIVILEGES") {
    failsToParse
  }

  test("SHOW USER user PRIVILEGES YIELD *, blah RETURN user") {
    val exceptionMessage =
      s"""Invalid input ',': expected
         |  "LIMIT"
         |  "ORDER"
         |  "RETURN"
         |  "SKIP"
         |  "WHERE"
         |  <EOF> (line 1, column 34 (offset: 33))""".stripMargin

    assertFailsWithMessage(testName, exceptionMessage)
  }

  test("SHOW USER user PRIVILEGES YIELD # RETURN user") {
    failsToParse
  }

  test("SHOW PRIVILEGES COMMANDS") {
    failsToParse
  }

  test("SHOW PRIVILEGES REVOKE") {
    failsToParse
  }

  test("SHOW PRIVILEGES AS REVOKE COMMAND COMMANDS") {
    failsToParse
  }

  test("SHOW PRIVILEGES AS COMMANDS REVOKE") {
    failsToParse
  }

  test("SHOW PRIVILEGES AS COMMANDS USER user") {
    failsToParse
  }
}
