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

class AllGraphPrivilegeAdministrationCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {

  Seq(
    ("GRANT", "TO", grantGraphPrivilege: noResourcePrivilegeFunc),
    ("DENY", "TO", denyGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE", "FROM", revokeGraphPrivilege: noResourcePrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: noResourcePrivilegeFunc) =>
      // All versions of ALL [[GRAPH] PRIVILEGES] should be allowed

      test(s"$verb ALL ON GRAPH foo $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(graphScopeFoo))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL PRIVILEGES ON GRAPH foo $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(graphScopeFoo))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPH foo $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(graphScopeFoo))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      // Home graph should be allowed

      test(s"$verb ALL ON HOME GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(ast.HomeGraphScope()(_)))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL PRIVILEGES ON HOME GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(ast.HomeGraphScope()(_)))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON HOME GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(ast.HomeGraphScope()(_)))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      // Default graph should be allowed

      test(s"$verb ALL ON DEFAULT GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(ast.DefaultGraphScope()(_)))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL PRIVILEGES ON DEFAULT GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(ast.DefaultGraphScope()(_)))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON DEFAULT GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(ast.DefaultGraphScope()(_)))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      // Multiple graphs should be allowed

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPHS * $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(ast.AllGraphsScope()(_)))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPHS foo,baz $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(graphScopeFoo, graphScopeBaz))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      // Multiple roles should be allowed

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPHS foo $preposition role1, role2") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(graphScopeFoo))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole1, literalRole2)
        ))
      }

      // Parameter values should be allowed

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPH $$foo $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(graphScopeParamFoo))(_),
          List(ast.AllQualifier()(_)),
          Seq(literalRole)
        ))
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPH foo $preposition $$role") {
        yields(func(
          ast.GraphPrivilege(ast.AllGraphAction, List(graphScopeFoo))(_),
          List(ast.AllQualifier()(_)),
          Seq(paramRole)
        ))
      }

      // Qualifier or resource should not be supported

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPH foo NODE A $preposition role") {
        failsToParse
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON GRAPH foo ELEMENTS * $preposition role") {
        failsToParse
      }

      test(s"$verb ALL GRAPH PRIVILEGES {prop} ON GRAPH foo $preposition role") {
        failsToParse
      }

      // Invalid syntax

      test(s"$verb ALL GRAPH ON GRAPH foo $preposition role") {
        failsToParse
      }

      test(s"$verb GRAPH ON GRAPH foo $preposition role") {
        val expected =
          """Invalid input 'GRAPH': expected
            |  "ACCESS"""".stripMargin
        assertFailsWithMessageStart(testName, expected)
      }

      test(s"$verb GRAPH PRIVILEGES ON GRAPH foo $preposition role") {
        val expected =
          """Invalid input 'GRAPH': expected
            |  "ACCESS"""".stripMargin
        assertFailsWithMessageStart(testName, expected)
      }

      test(s"$verb PRIVILEGES ON GRAPH foo $preposition role") {
        val expected =
          """Invalid input 'PRIVILEGES': expected
            |  "ACCESS"""".stripMargin
        assertFailsWithMessageStart(testName, expected)
      }

      // Database/dbms instead of graph keyword

      test(s"$verb ALL GRAPH PRIVILEGES ON DATABASES * $preposition role") {
        val offset = verb.length + 25
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'DATABASES': expected "GRAPH" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON DATABASE foo $preposition role") {
        val offset = verb.length + 25
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'DATABASE': expected "GRAPH" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON HOME DATABASE $preposition role") {
        val offset = verb.length + 25
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'HOME': expected "GRAPH" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON DEFAULT DATABASE $preposition role") {
        val offset = verb.length + 25
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'DEFAULT': expected "GRAPH" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      test(s"$verb ALL GRAPH PRIVILEGES ON DBMS $preposition role") {
        val offset = verb.length + 25
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'DBMS': expected "GRAPH" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }
  }

}
