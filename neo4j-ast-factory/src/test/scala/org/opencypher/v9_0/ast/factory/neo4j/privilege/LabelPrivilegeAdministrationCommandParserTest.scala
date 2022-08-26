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

class LabelPrivilegeAdministrationCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {
  private val labelResource = ast.LabelsResource(Seq("label"))(_)

  Seq(
    ("GRANT", "TO", grantGraphPrivilege: resourcePrivilegeFunc),
    ("DENY", "TO", denyGraphPrivilege: resourcePrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantGraphPrivilege: resourcePrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyGraphPrivilege: resourcePrivilegeFunc),
    ("REVOKE", "FROM", revokeGraphPrivilege: resourcePrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: resourcePrivilegeFunc) =>
      Seq[Immutable](true, false).foreach {
        immutable =>
          val immutableString = immutableOrEmpty(immutable)
          Seq(
            ("SET", ast.SetLabelAction),
            ("REMOVE", ast.RemoveLabelAction)
          ).foreach {
            case (setOrRemove, action) =>
              test(s"$verb$immutableString $setOrRemove LABEL label ON GRAPH foo $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(graphScopeFoo))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              // Multiple labels should be allowed

              test(s"$verb$immutableString $setOrRemove LABEL * ON GRAPH foo $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(graphScopeFoo))(_),
                  ast.AllLabelResource()(_),
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $setOrRemove LABEL label1, label2 ON GRAPH foo $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(graphScopeFoo))(_),
                  ast.LabelsResource(Seq("label1", "label2"))(_),
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              // Multiple graphs should be allowed

              test(s"$verb$immutableString $setOrRemove LABEL label ON GRAPHS * $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(ast.AllGraphsScope()(_)))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $setOrRemove LABEL label ON GRAPHS foo,baz $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(graphScopeFoo, graphScopeBaz))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              // Home graph should be allowed

              test(s"$verb$immutableString $setOrRemove LABEL label ON HOME GRAPH $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(ast.HomeGraphScope()(_)))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $setOrRemove LABEL * ON HOME GRAPH $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(ast.HomeGraphScope()(_)))(_),
                  ast.AllLabelResource()(_),
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              // Default graph should be allowed

              test(s"$verb$immutableString $setOrRemove LABEL label ON DEFAULT GRAPH $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(ast.DefaultGraphScope()(_)))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $setOrRemove LABEL * ON DEFAULT GRAPH $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(ast.DefaultGraphScope()(_)))(_),
                  ast.AllLabelResource()(_),
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              // Multiple roles should be allowed

              test(s"$verb$immutableString $setOrRemove LABEL label ON GRAPHS foo $preposition role1, role2") {
                yields(func(
                  ast.GraphPrivilege(action, List(graphScopeFoo))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole1, literalRole2),
                  immutable
                ))
              }

              // Parameter values

              test(s"$verb$immutableString $setOrRemove LABEL label ON GRAPH $$foo $preposition role") {
                yields(func(
                  ast.GraphPrivilege(action, List(graphScopeParamFoo))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(literalRole),
                  immutable
                ))
              }

              test(s"$verb$immutableString $setOrRemove LABEL label ON GRAPH foo $preposition $$role") {
                yields(func(
                  ast.GraphPrivilege(action, List(graphScopeFoo))(_),
                  labelResource,
                  List(ast.LabelAllQualifier()(_)),
                  Seq(paramRole),
                  immutable
                ))
              }

              // TODO: should this one be supported?
              test(s"$verb$immutableString $setOrRemove LABEL $$label ON GRAPH foo $preposition role") {
                failsToParse
              }

              // LABELS instead of LABEL

              test(s"$verb$immutableString $setOrRemove LABELS label ON GRAPH * $preposition role") {
                assertFailsWithMessageStart(testName, s"""Invalid input 'LABELS': expected""")
              }

              // Database instead of graph keyword

              test(s"$verb$immutableString $setOrRemove LABEL label ON DATABASES * $preposition role") {
                assertFailsWithMessageStart(testName, s"""Invalid input 'DATABASES': expected""")
              }

              test(s"$verb$immutableString $setOrRemove LABEL label ON DATABASE foo $preposition role") {
                assertFailsWithMessageStart(testName, s"""Invalid input 'DATABASE': expected""")
              }

              test(s"$verb$immutableString $setOrRemove LABEL label ON HOME DATABASE $preposition role") {
                failsToParse
              }

              test(s"$verb$immutableString $setOrRemove LABEL label ON DEFAULT DATABASE $preposition role") {
                failsToParse
              }
          }
      }
  }
}
