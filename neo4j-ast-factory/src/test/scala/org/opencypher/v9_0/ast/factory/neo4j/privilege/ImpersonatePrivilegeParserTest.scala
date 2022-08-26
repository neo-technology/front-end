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
import org.opencypher.v9_0.expressions.ExplicitParameter
import org.opencypher.v9_0.expressions.Parameter
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.CTString

class ImpersonatePrivilegeParserTest extends AdministrationAndSchemaCommandParserTestBase {

  type impersonatePrivilegeFunc =
    (List[ast.PrivilegeQualifier], Seq[Either[String, Parameter]], Immutable) => InputPosition => ast.Statement

  def grantImpersonatePrivilege(
    q: List[ast.PrivilegeQualifier],
    r: Seq[Either[String, Parameter]],
    i: Immutable
  ): InputPosition => ast.Statement =
    ast.GrantPrivilege.dbmsAction(ast.ImpersonateUserAction, i, r, q)

  def denyImpersonatePrivilege(
    q: List[ast.PrivilegeQualifier],
    r: Seq[Either[String, Parameter]],
    i: Immutable
  ): InputPosition => ast.Statement =
    ast.DenyPrivilege.dbmsAction(ast.ImpersonateUserAction, i, r, q)

  def revokeGrantImpersonatePrivilege(
    q: List[ast.PrivilegeQualifier],
    r: Seq[Either[String, Parameter]],
    i: Immutable
  ): InputPosition => ast.Statement =
    ast.RevokePrivilege.dbmsAction(ast.ImpersonateUserAction, i, r, ast.RevokeGrantType()(pos), q)

  def revokeDenyImpersonatePrivilege(
    q: List[ast.PrivilegeQualifier],
    r: Seq[Either[String, Parameter]],
    i: Immutable
  ): InputPosition => ast.Statement =
    ast.RevokePrivilege.dbmsAction(ast.ImpersonateUserAction, i, r, ast.RevokeDenyType()(pos), q)

  def revokeImpersonatePrivilege(
    q: List[ast.PrivilegeQualifier],
    r: Seq[Either[String, Parameter]],
    i: Immutable
  ): InputPosition => ast.Statement =
    ast.RevokePrivilege.dbmsAction(ast.ImpersonateUserAction, i, r, ast.RevokeBothType()(pos), q)

  Seq(
    ("GRANT", "TO", grantImpersonatePrivilege: impersonatePrivilegeFunc),
    ("DENY", "TO", denyImpersonatePrivilege: impersonatePrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantImpersonatePrivilege: impersonatePrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyImpersonatePrivilege: impersonatePrivilegeFunc),
    ("REVOKE", "FROM", revokeImpersonatePrivilege: impersonatePrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: impersonatePrivilegeFunc) =>
      Seq[Immutable](true, false).foreach {
        immutable =>
          val immutableString = immutableOrEmpty(immutable)
          test(s"$verb$immutableString IMPERSONATE ON DBMS $preposition role") {
            assertAst(
              func(List(ast.UserAllQualifier()(pos)), List(Left("role")), immutable)(defaultPos),
              comparePosition = false
            )
          }

          test(s"$verb$immutableString IMPERSONATE (*) ON DBMS $preposition role") {
            assertAst(
              func(List(ast.UserAllQualifier()(pos)), List(Left("role")), immutable)(defaultPos),
              comparePosition = false
            )
          }

          test(s"$verb$immutableString IMPERSONATE (foo) ON DBMS $preposition role") {
            assertAst(
              func(List(ast.UserQualifier(Left("foo"))(pos)), List(Left("role")), immutable)(defaultPos),
              comparePosition = false
            )
          }

          test(s"$verb$immutableString IMPERSONATE (foo, $$userParam) ON DBMS $preposition role") {
            val fooColumn: Int = verb.length + immutableString.length + " IMPERSONATE (".length
            val useParamColumn: Int = fooColumn + "foo $".length
            assertAst(func(
              List(
                ast.UserQualifier(Left("foo"))((1, fooColumn + 1, fooColumn)),
                ast.UserQualifier(Right(ExplicitParameter("userParam", CTString)((
                  1,
                  useParamColumn + 1,
                  useParamColumn
                ))))(
                  defaultPos
                )
              ),
              List(Left("role")),
              immutable
            )(defaultPos))
          }
      }
  }
}
