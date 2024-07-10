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
package org.opencypher.v9_0.ast.generator

import org.opencypher.v9_0.ast.Access
import org.opencypher.v9_0.ast.AccessDatabaseAction
import org.opencypher.v9_0.ast.ActionResource
import org.opencypher.v9_0.ast.AdministrationCommand
import org.opencypher.v9_0.ast.AliasedReturnItem
import org.opencypher.v9_0.ast.AllAliasManagementActions
import org.opencypher.v9_0.ast.AllConstraintActions
import org.opencypher.v9_0.ast.AllConstraints
import org.opencypher.v9_0.ast.AllDatabaseAction
import org.opencypher.v9_0.ast.AllDatabaseManagementActions
import org.opencypher.v9_0.ast.AllDatabasesQualifier
import org.opencypher.v9_0.ast.AllDatabasesScope
import org.opencypher.v9_0.ast.AllDbmsAction
import org.opencypher.v9_0.ast.AllFunctions
import org.opencypher.v9_0.ast.AllGraphAction
import org.opencypher.v9_0.ast.AllGraphsScope
import org.opencypher.v9_0.ast.AllIndexActions
import org.opencypher.v9_0.ast.AllIndexes
import org.opencypher.v9_0.ast.AllLabelResource
import org.opencypher.v9_0.ast.AllPrivilegeActions
import org.opencypher.v9_0.ast.AllPropertyResource
import org.opencypher.v9_0.ast.AllQualifier
import org.opencypher.v9_0.ast.AllRoleActions
import org.opencypher.v9_0.ast.AllTokenActions
import org.opencypher.v9_0.ast.AllTransactionActions
import org.opencypher.v9_0.ast.AllUserActions
import org.opencypher.v9_0.ast.AlterAliasAction
import org.opencypher.v9_0.ast.AlterDatabase
import org.opencypher.v9_0.ast.AlterDatabaseAction
import org.opencypher.v9_0.ast.AlterLocalDatabaseAlias
import org.opencypher.v9_0.ast.AlterRemoteDatabaseAlias
import org.opencypher.v9_0.ast.AlterUser
import org.opencypher.v9_0.ast.AlterUserAction
import org.opencypher.v9_0.ast.AscSortItem
import org.opencypher.v9_0.ast.AssignPrivilegeAction
import org.opencypher.v9_0.ast.AssignRoleAction
import org.opencypher.v9_0.ast.BuiltInFunctions
import org.opencypher.v9_0.ast.Clause
import org.opencypher.v9_0.ast.CommandResultItem
import org.opencypher.v9_0.ast.CompositeDatabaseManagementActions
import org.opencypher.v9_0.ast.ConstraintVersion2
import org.opencypher.v9_0.ast.Create
import org.opencypher.v9_0.ast.CreateAliasAction
import org.opencypher.v9_0.ast.CreateCompositeDatabase
import org.opencypher.v9_0.ast.CreateCompositeDatabaseAction
import org.opencypher.v9_0.ast.CreateConstraintAction
import org.opencypher.v9_0.ast.CreateDatabase
import org.opencypher.v9_0.ast.CreateDatabaseAction
import org.opencypher.v9_0.ast.CreateElementAction
import org.opencypher.v9_0.ast.CreateFulltextNodeIndex
import org.opencypher.v9_0.ast.CreateFulltextRelationshipIndex
import org.opencypher.v9_0.ast.CreateIndex
import org.opencypher.v9_0.ast.CreateIndexAction
import org.opencypher.v9_0.ast.CreateLocalDatabaseAlias
import org.opencypher.v9_0.ast.CreateLookupIndex
import org.opencypher.v9_0.ast.CreateNodeKeyConstraint
import org.opencypher.v9_0.ast.CreateNodeLabelAction
import org.opencypher.v9_0.ast.CreateNodePropertyExistenceConstraint
import org.opencypher.v9_0.ast.CreatePointNodeIndex
import org.opencypher.v9_0.ast.CreatePointRelationshipIndex
import org.opencypher.v9_0.ast.CreatePropertyKeyAction
import org.opencypher.v9_0.ast.CreateRangeNodeIndex
import org.opencypher.v9_0.ast.CreateRangeRelationshipIndex
import org.opencypher.v9_0.ast.CreateRelationshipPropertyExistenceConstraint
import org.opencypher.v9_0.ast.CreateRelationshipTypeAction
import org.opencypher.v9_0.ast.CreateRemoteDatabaseAlias
import org.opencypher.v9_0.ast.CreateRole
import org.opencypher.v9_0.ast.CreateRoleAction
import org.opencypher.v9_0.ast.CreateTextNodeIndex
import org.opencypher.v9_0.ast.CreateTextRelationshipIndex
import org.opencypher.v9_0.ast.CreateUniquePropertyConstraint
import org.opencypher.v9_0.ast.CreateUser
import org.opencypher.v9_0.ast.CreateUserAction
import org.opencypher.v9_0.ast.CurrentUser
import org.opencypher.v9_0.ast.DatabaseAction
import org.opencypher.v9_0.ast.DatabaseName
import org.opencypher.v9_0.ast.DatabasePrivilegeQualifier
import org.opencypher.v9_0.ast.DbmsAction
import org.opencypher.v9_0.ast.DeallocateServers
import org.opencypher.v9_0.ast.DefaultDatabaseScope
import org.opencypher.v9_0.ast.DefaultGraphScope
import org.opencypher.v9_0.ast.Delete
import org.opencypher.v9_0.ast.DeleteElementAction
import org.opencypher.v9_0.ast.DenyPrivilege
import org.opencypher.v9_0.ast.DescSortItem
import org.opencypher.v9_0.ast.DestroyData
import org.opencypher.v9_0.ast.DropAliasAction
import org.opencypher.v9_0.ast.DropCompositeDatabaseAction
import org.opencypher.v9_0.ast.DropConstraintAction
import org.opencypher.v9_0.ast.DropConstraintOnName
import org.opencypher.v9_0.ast.DropDatabase
import org.opencypher.v9_0.ast.DropDatabaseAction
import org.opencypher.v9_0.ast.DropDatabaseAlias
import org.opencypher.v9_0.ast.DropIndexAction
import org.opencypher.v9_0.ast.DropIndexOnName
import org.opencypher.v9_0.ast.DropRole
import org.opencypher.v9_0.ast.DropRoleAction
import org.opencypher.v9_0.ast.DropServer
import org.opencypher.v9_0.ast.DropUser
import org.opencypher.v9_0.ast.DropUserAction
import org.opencypher.v9_0.ast.DumpData
import org.opencypher.v9_0.ast.ElementQualifier
import org.opencypher.v9_0.ast.ElementsAllQualifier
import org.opencypher.v9_0.ast.EnableServer
import org.opencypher.v9_0.ast.ExecuteAdminProcedureAction
import org.opencypher.v9_0.ast.ExecuteBoostedFunctionAction
import org.opencypher.v9_0.ast.ExecuteBoostedProcedureAction
import org.opencypher.v9_0.ast.ExecuteFunctionAction
import org.opencypher.v9_0.ast.ExecuteProcedureAction
import org.opencypher.v9_0.ast.ExistsConstraints
import org.opencypher.v9_0.ast.Foreach
import org.opencypher.v9_0.ast.FulltextIndexes
import org.opencypher.v9_0.ast.FunctionQualifier
import org.opencypher.v9_0.ast.GrantPrivilege
import org.opencypher.v9_0.ast.GrantRolesToUsers
import org.opencypher.v9_0.ast.GraphAction
import org.opencypher.v9_0.ast.GraphPrivilegeQualifier
import org.opencypher.v9_0.ast.HomeDatabaseScope
import org.opencypher.v9_0.ast.HomeGraphScope
import org.opencypher.v9_0.ast.IfExistsDo
import org.opencypher.v9_0.ast.IfExistsDoNothing
import org.opencypher.v9_0.ast.IfExistsInvalidSyntax
import org.opencypher.v9_0.ast.IfExistsReplace
import org.opencypher.v9_0.ast.IfExistsThrowError
import org.opencypher.v9_0.ast.ImpersonateUserAction
import org.opencypher.v9_0.ast.IndefiniteWait
import org.opencypher.v9_0.ast.LabelAllQualifier
import org.opencypher.v9_0.ast.LabelQualifier
import org.opencypher.v9_0.ast.LabelsResource
import org.opencypher.v9_0.ast.Limit
import org.opencypher.v9_0.ast.LoadCSV
import org.opencypher.v9_0.ast.LookupIndexes
import org.opencypher.v9_0.ast.Match
import org.opencypher.v9_0.ast.MatchAction
import org.opencypher.v9_0.ast.Merge
import org.opencypher.v9_0.ast.MergeAction
import org.opencypher.v9_0.ast.MergeAdminAction
import org.opencypher.v9_0.ast.NamedDatabaseScope
import org.opencypher.v9_0.ast.NamedGraphScope
import org.opencypher.v9_0.ast.NamespacedName
import org.opencypher.v9_0.ast.NoOptions
import org.opencypher.v9_0.ast.NoWait
import org.opencypher.v9_0.ast.NodeExistsConstraints
import org.opencypher.v9_0.ast.NodeKeyConstraints
import org.opencypher.v9_0.ast.OnCreate
import org.opencypher.v9_0.ast.OnMatch
import org.opencypher.v9_0.ast.Options
import org.opencypher.v9_0.ast.OptionsMap
import org.opencypher.v9_0.ast.OptionsParam
import org.opencypher.v9_0.ast.OrderBy
import org.opencypher.v9_0.ast.ParameterName
import org.opencypher.v9_0.ast.ParsedAsYield
import org.opencypher.v9_0.ast.PointIndexes
import org.opencypher.v9_0.ast.PrivilegeCommand
import org.opencypher.v9_0.ast.PrivilegeQualifier
import org.opencypher.v9_0.ast.ProcedureQualifier
import org.opencypher.v9_0.ast.ProcedureResult
import org.opencypher.v9_0.ast.ProcedureResultItem
import org.opencypher.v9_0.ast.PropertiesResource
import org.opencypher.v9_0.ast.Query
import org.opencypher.v9_0.ast.QueryPart
import org.opencypher.v9_0.ast.RangeIndexes
import org.opencypher.v9_0.ast.ReadAction
import org.opencypher.v9_0.ast.ReadOnlyAccess
import org.opencypher.v9_0.ast.ReadWriteAccess
import org.opencypher.v9_0.ast.RelExistsConstraints
import org.opencypher.v9_0.ast.RelationshipAllQualifier
import org.opencypher.v9_0.ast.RelationshipQualifier
import org.opencypher.v9_0.ast.Remove
import org.opencypher.v9_0.ast.RemoveHomeDatabaseAction
import org.opencypher.v9_0.ast.RemoveItem
import org.opencypher.v9_0.ast.RemoveLabelAction
import org.opencypher.v9_0.ast.RemoveLabelItem
import org.opencypher.v9_0.ast.RemovePrivilegeAction
import org.opencypher.v9_0.ast.RemovePropertyItem
import org.opencypher.v9_0.ast.RemoveRoleAction
import org.opencypher.v9_0.ast.RenameRole
import org.opencypher.v9_0.ast.RenameRoleAction
import org.opencypher.v9_0.ast.RenameServer
import org.opencypher.v9_0.ast.RenameUser
import org.opencypher.v9_0.ast.RenameUserAction
import org.opencypher.v9_0.ast.Return
import org.opencypher.v9_0.ast.ReturnItem
import org.opencypher.v9_0.ast.ReturnItems
import org.opencypher.v9_0.ast.RevokeBothType
import org.opencypher.v9_0.ast.RevokeDenyType
import org.opencypher.v9_0.ast.RevokeGrantType
import org.opencypher.v9_0.ast.RevokePrivilege
import org.opencypher.v9_0.ast.RevokeRolesFromUsers
import org.opencypher.v9_0.ast.RevokeType
import org.opencypher.v9_0.ast.SchemaCommand
import org.opencypher.v9_0.ast.SeekOnly
import org.opencypher.v9_0.ast.SeekOrScan
import org.opencypher.v9_0.ast.ServerManagementAction
import org.opencypher.v9_0.ast.SetClause
import org.opencypher.v9_0.ast.SetDatabaseAccessAction
import org.opencypher.v9_0.ast.SetExactPropertiesFromMapItem
import org.opencypher.v9_0.ast.SetHomeDatabaseAction
import org.opencypher.v9_0.ast.SetIncludingPropertiesFromMapItem
import org.opencypher.v9_0.ast.SetItem
import org.opencypher.v9_0.ast.SetLabelAction
import org.opencypher.v9_0.ast.SetLabelItem
import org.opencypher.v9_0.ast.SetOwnPassword
import org.opencypher.v9_0.ast.SetPasswordsAction
import org.opencypher.v9_0.ast.SetPropertyAction
import org.opencypher.v9_0.ast.SetPropertyItem
import org.opencypher.v9_0.ast.SetUserHomeDatabaseAction
import org.opencypher.v9_0.ast.SetUserStatusAction
import org.opencypher.v9_0.ast.ShowAliasAction
import org.opencypher.v9_0.ast.ShowAliases
import org.opencypher.v9_0.ast.ShowAllPrivileges
import org.opencypher.v9_0.ast.ShowConstraintAction
import org.opencypher.v9_0.ast.ShowConstraintType
import org.opencypher.v9_0.ast.ShowConstraintsClause
import org.opencypher.v9_0.ast.ShowCurrentUser
import org.opencypher.v9_0.ast.ShowDatabase
import org.opencypher.v9_0.ast.ShowFunctionsClause
import org.opencypher.v9_0.ast.ShowIndexAction
import org.opencypher.v9_0.ast.ShowIndexType
import org.opencypher.v9_0.ast.ShowIndexesClause
import org.opencypher.v9_0.ast.ShowPrivilegeAction
import org.opencypher.v9_0.ast.ShowPrivilegeCommands
import org.opencypher.v9_0.ast.ShowPrivileges
import org.opencypher.v9_0.ast.ShowProceduresClause
import org.opencypher.v9_0.ast.ShowRoleAction
import org.opencypher.v9_0.ast.ShowRoles
import org.opencypher.v9_0.ast.ShowRolesPrivileges
import org.opencypher.v9_0.ast.ShowServerAction
import org.opencypher.v9_0.ast.ShowServers
import org.opencypher.v9_0.ast.ShowTransactionAction
import org.opencypher.v9_0.ast.ShowTransactionsClause
import org.opencypher.v9_0.ast.ShowUserAction
import org.opencypher.v9_0.ast.ShowUserPrivileges
import org.opencypher.v9_0.ast.ShowUsers
import org.opencypher.v9_0.ast.ShowUsersPrivileges
import org.opencypher.v9_0.ast.SingleQuery
import org.opencypher.v9_0.ast.Skip
import org.opencypher.v9_0.ast.SortItem
import org.opencypher.v9_0.ast.StartDatabase
import org.opencypher.v9_0.ast.StartDatabaseAction
import org.opencypher.v9_0.ast.Statement
import org.opencypher.v9_0.ast.StopDatabase
import org.opencypher.v9_0.ast.StopDatabaseAction
import org.opencypher.v9_0.ast.SubqueryCall
import org.opencypher.v9_0.ast.SubqueryCall.InTransactionsParameters
import org.opencypher.v9_0.ast.TerminateTransactionAction
import org.opencypher.v9_0.ast.TerminateTransactionsClause
import org.opencypher.v9_0.ast.TextIndexes
import org.opencypher.v9_0.ast.TimeoutAfter
import org.opencypher.v9_0.ast.TransactionManagementAction
import org.opencypher.v9_0.ast.TraverseAction
import org.opencypher.v9_0.ast.UnaliasedReturnItem
import org.opencypher.v9_0.ast.Union
import org.opencypher.v9_0.ast.UnionAll
import org.opencypher.v9_0.ast.UnionDistinct
import org.opencypher.v9_0.ast.UniqueConstraints
import org.opencypher.v9_0.ast.UnresolvedCall
import org.opencypher.v9_0.ast.Unwind
import org.opencypher.v9_0.ast.UseGraph
import org.opencypher.v9_0.ast.User
import org.opencypher.v9_0.ast.UserAllQualifier
import org.opencypher.v9_0.ast.UserDefinedFunctions
import org.opencypher.v9_0.ast.UserOptions
import org.opencypher.v9_0.ast.UserQualifier
import org.opencypher.v9_0.ast.UsingAnyIndexType
import org.opencypher.v9_0.ast.UsingHint
import org.opencypher.v9_0.ast.UsingIndexHint
import org.opencypher.v9_0.ast.UsingJoinHint
import org.opencypher.v9_0.ast.UsingPointIndexType
import org.opencypher.v9_0.ast.UsingRangeIndexType
import org.opencypher.v9_0.ast.UsingScanHint
import org.opencypher.v9_0.ast.UsingTextIndexType
import org.opencypher.v9_0.ast.ValidSyntax
import org.opencypher.v9_0.ast.WaitUntilComplete
import org.opencypher.v9_0.ast.Where
import org.opencypher.v9_0.ast.With
import org.opencypher.v9_0.ast.WriteAction
import org.opencypher.v9_0.ast.Yield
import org.opencypher.v9_0.ast.YieldOrWhere
import org.opencypher.v9_0.ast.generator.AstGenerator.boolean
import org.opencypher.v9_0.ast.generator.AstGenerator.char
import org.opencypher.v9_0.ast.generator.AstGenerator.oneOrMore
import org.opencypher.v9_0.ast.generator.AstGenerator.tuple
import org.opencypher.v9_0.ast.generator.AstGenerator.twoOrMore
import org.opencypher.v9_0.ast.generator.AstGenerator.zeroOrMore
import org.opencypher.v9_0.expressions.Add
import org.opencypher.v9_0.expressions.AllIterablePredicate
import org.opencypher.v9_0.expressions.AllPropertiesSelector
import org.opencypher.v9_0.expressions.And
import org.opencypher.v9_0.expressions.Ands
import org.opencypher.v9_0.expressions.AnonymousPatternPart
import org.opencypher.v9_0.expressions.AnyIterablePredicate
import org.opencypher.v9_0.expressions.BooleanLiteral
import org.opencypher.v9_0.expressions.CaseExpression
import org.opencypher.v9_0.expressions.ContainerIndex
import org.opencypher.v9_0.expressions.Contains
import org.opencypher.v9_0.expressions.CountExpression
import org.opencypher.v9_0.expressions.CountStar
import org.opencypher.v9_0.expressions.DecimalDoubleLiteral
import org.opencypher.v9_0.expressions.Divide
import org.opencypher.v9_0.expressions.EndsWith
import org.opencypher.v9_0.expressions.EntityType
import org.opencypher.v9_0.expressions.Equals
import org.opencypher.v9_0.expressions.EveryPath
import org.opencypher.v9_0.expressions.ExistsExpression
import org.opencypher.v9_0.expressions.ExplicitParameter
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.ExtractScope
import org.opencypher.v9_0.expressions.False
import org.opencypher.v9_0.expressions.FilterScope
import org.opencypher.v9_0.expressions.FixedQuantifier
import org.opencypher.v9_0.expressions.FunctionInvocation
import org.opencypher.v9_0.expressions.FunctionName
import org.opencypher.v9_0.expressions.GraphPatternQuantifier
import org.opencypher.v9_0.expressions.GreaterThan
import org.opencypher.v9_0.expressions.GreaterThanOrEqual
import org.opencypher.v9_0.expressions.In
import org.opencypher.v9_0.expressions.Infinity
import org.opencypher.v9_0.expressions.IntervalQuantifier
import org.opencypher.v9_0.expressions.InvalidNotEquals
import org.opencypher.v9_0.expressions.IsNotNull
import org.opencypher.v9_0.expressions.IsNull
import org.opencypher.v9_0.expressions.IterablePredicateExpression
import org.opencypher.v9_0.expressions.LabelExpression
import org.opencypher.v9_0.expressions.LabelExpression.Leaf
import org.opencypher.v9_0.expressions.LabelName
import org.opencypher.v9_0.expressions.LabelOrRelTypeName
import org.opencypher.v9_0.expressions.LessThan
import org.opencypher.v9_0.expressions.LessThanOrEqual
import org.opencypher.v9_0.expressions.ListComprehension
import org.opencypher.v9_0.expressions.ListLiteral
import org.opencypher.v9_0.expressions.ListSlice
import org.opencypher.v9_0.expressions.Literal
import org.opencypher.v9_0.expressions.LiteralEntry
import org.opencypher.v9_0.expressions.MapExpression
import org.opencypher.v9_0.expressions.MapProjection
import org.opencypher.v9_0.expressions.MapProjectionElement
import org.opencypher.v9_0.expressions.Modulo
import org.opencypher.v9_0.expressions.Multiply
import org.opencypher.v9_0.expressions.NODE_TYPE
import org.opencypher.v9_0.expressions.NaN
import org.opencypher.v9_0.expressions.NamedPatternPart
import org.opencypher.v9_0.expressions.Namespace
import org.opencypher.v9_0.expressions.NodePattern
import org.opencypher.v9_0.expressions.NoneIterablePredicate
import org.opencypher.v9_0.expressions.Not
import org.opencypher.v9_0.expressions.NotEquals
import org.opencypher.v9_0.expressions.Null
import org.opencypher.v9_0.expressions.Or
import org.opencypher.v9_0.expressions.Parameter
import org.opencypher.v9_0.expressions.PathConcatenation
import org.opencypher.v9_0.expressions.PathFactor
import org.opencypher.v9_0.expressions.Pattern
import org.opencypher.v9_0.expressions.PatternComprehension
import org.opencypher.v9_0.expressions.PatternElement
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.PatternPart
import org.opencypher.v9_0.expressions.PlusQuantifier
import org.opencypher.v9_0.expressions.Pow
import org.opencypher.v9_0.expressions.ProcedureName
import org.opencypher.v9_0.expressions.ProcedureOutput
import org.opencypher.v9_0.expressions.Property
import org.opencypher.v9_0.expressions.PropertyKeyName
import org.opencypher.v9_0.expressions.PropertySelector
import org.opencypher.v9_0.expressions.QuantifiedPath
import org.opencypher.v9_0.expressions.RELATIONSHIP_TYPE
import org.opencypher.v9_0.expressions.Range
import org.opencypher.v9_0.expressions.ReduceExpression
import org.opencypher.v9_0.expressions.ReduceScope
import org.opencypher.v9_0.expressions.RegexMatch
import org.opencypher.v9_0.expressions.RelTypeName
import org.opencypher.v9_0.expressions.RelationshipChain
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.RelationshipsPattern
import org.opencypher.v9_0.expressions.SemanticDirection
import org.opencypher.v9_0.expressions.SensitiveAutoParameter
import org.opencypher.v9_0.expressions.SensitiveParameter
import org.opencypher.v9_0.expressions.SensitiveStringLiteral
import org.opencypher.v9_0.expressions.ShortestPathExpression
import org.opencypher.v9_0.expressions.ShortestPaths
import org.opencypher.v9_0.expressions.SignedDecimalIntegerLiteral
import org.opencypher.v9_0.expressions.SignedHexIntegerLiteral
import org.opencypher.v9_0.expressions.SignedIntegerLiteral
import org.opencypher.v9_0.expressions.SignedOctalIntegerLiteral
import org.opencypher.v9_0.expressions.SimplePattern
import org.opencypher.v9_0.expressions.SingleIterablePredicate
import org.opencypher.v9_0.expressions.StarQuantifier
import org.opencypher.v9_0.expressions.StartsWith
import org.opencypher.v9_0.expressions.StringLiteral
import org.opencypher.v9_0.expressions.Subtract
import org.opencypher.v9_0.expressions.True
import org.opencypher.v9_0.expressions.UnaryAdd
import org.opencypher.v9_0.expressions.UnarySubtract
import org.opencypher.v9_0.expressions.UnsignedDecimalIntegerLiteral
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.expressions.VariableSelector
import org.opencypher.v9_0.expressions.Xor
import org.opencypher.v9_0.expressions.functions.Labels
import org.opencypher.v9_0.expressions.functions.Type
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.AnyType
import org.opencypher.v9_0.util.symbols.CTMap
import org.opencypher.v9_0.util.symbols.CTString
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.alphaLowerChar
import org.scalacheck.Gen.choose
import org.scalacheck.Gen.const
import org.scalacheck.Gen.frequency
import org.scalacheck.Gen.listOf
import org.scalacheck.Gen.listOfN
import org.scalacheck.Gen.lzy
import org.scalacheck.Gen.nonEmptyListOf
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen.option
import org.scalacheck.Gen.pick
import org.scalacheck.Gen.posNum
import org.scalacheck.Gen.sequence
import org.scalacheck.Gen.some
import org.scalacheck.util.Buildable

import java.nio.charset.StandardCharsets

import scala.util.Random

object AstGenerator {
  val OR_MORE_UPPER_BOUND = 3

  def zeroOrMore[T](gen: Gen[T]): Gen[List[T]] =
    choose(0, OR_MORE_UPPER_BOUND).flatMap(listOfN(_, gen))

  def zeroOrMore[T](seq: Seq[T]): Gen[Seq[T]] =
    choose(0, Math.min(OR_MORE_UPPER_BOUND, seq.size)).flatMap(pick(_, seq)).map(_.toSeq)

  def oneOrMore[T](gen: Gen[T]): Gen[List[T]] =
    choose(1, OR_MORE_UPPER_BOUND).flatMap(listOfN(_, gen))

  def oneOrMore[T](seq: Seq[T]): Gen[Seq[T]] =
    choose(1, Math.min(OR_MORE_UPPER_BOUND, seq.size)).flatMap(pick(_, seq)).map(_.toSeq)

  def twoOrMore[T](gen: Gen[T]): Gen[List[T]] =
    choose(2, OR_MORE_UPPER_BOUND).flatMap(listOfN(_, gen))

  def twoOrMore[T](seq: Seq[T]): Gen[Seq[T]] =
    choose(2, Math.min(OR_MORE_UPPER_BOUND, seq.size)).flatMap(pick(_, seq)).map(_.toSeq)

  def tuple[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = for {
    a <- ga
    b <- gb
  } yield (a, b)

  def boolean: Gen[Boolean] =
    Arbitrary.arbBool.arbitrary

  def char: Gen[Char] =
    Arbitrary.arbChar.arbitrary.suchThat(acceptedChar)

  def acceptedChar(c: Char): Boolean = {
    val DEL_ERROR = '\ufdea'
    val INS_ERROR = '\ufdeb'
    val RESYNC = '\ufdec'
    val RESYNC_START = '\ufded'
    val RESYNC_END = '\ufdee'
    val RESYNC_EOI = '\ufdef'
    val EOI = '\uffff'

    c match {
      case DEL_ERROR    => false
      case INS_ERROR    => false
      case RESYNC       => false
      case RESYNC_START => false
      case RESYNC_END   => false
      case RESYNC_EOI   => false
      case EOI          => false
      case _            => true
    }
  }

}

/**
 * Random query generation
 * Implements instances of Gen[T] for all query ast nodes
 * Generated queries are syntactically (but not semantically) valid
 */
class AstGenerator(simpleStrings: Boolean = true, allowedVarNames: Option[Seq[String]] = None) {

  // HELPERS
  // ==========================================================================

  protected var paramCount = 0
  protected val pos: InputPosition = InputPosition.NONE

  def string: Gen[String] =
    if (simpleStrings) alphaLowerChar.map(_.toString)
    else listOf(char).map(_.mkString)

  // IDENTIFIERS
  // ==========================================================================

  def _identifier: Gen[String] =
    if (simpleStrings) alphaLowerChar.map(_.toString)
    else nonEmptyListOf(char).map(_.mkString)

  def _labelName: Gen[LabelName] =
    _identifier.map(LabelName(_)(pos))

  def _relTypeName: Gen[RelTypeName] =
    _identifier.map(RelTypeName(_)(pos))

  def _labelOrTypeName: Gen[LabelOrRelTypeName] =
    _identifier.map(LabelOrRelTypeName(_)(pos))

  def _propertyKeyName: Gen[PropertyKeyName] =
    _identifier.map(PropertyKeyName(_)(pos))

  // EXPRESSIONS
  // ==========================================================================

  // LEAFS
  // ----------------------------------

  def _nullLit: Gen[Null] =
    const(Null.NULL)

  def _stringLit: Gen[StringLiteral] =
    string.flatMap(StringLiteral(_)(pos))

  def _sensitiveStringLiteral: Gen[SensitiveStringLiteral] =
    // Needs to be '******' since all sensitive strings get rendered as such
    // Would normally get rewritten as SensitiveAutoParameter which can be generated as parameter when needed
    const(SensitiveStringLiteral("******".getBytes(StandardCharsets.UTF_8))(pos))

  def _booleanLit: Gen[BooleanLiteral] =
    oneOf(True()(pos), False()(pos))

  def _infinityLit: Gen[Literal] =
    const(Infinity()(pos))

  def _nanLit: Gen[Literal] =
    const(NaN()(pos))

  def _unsignedIntString(prefix: String, radix: Int): Gen[String] = for {
    num <- posNum[Int]
    str = Integer.toString(num, radix)
  } yield List(prefix, str).mkString

  def _signedIntString(prefix: String, radix: Int): Gen[String] = for {
    str <- _unsignedIntString(prefix, radix)
    neg <- boolean
    sig = if (neg) "-" else ""
  } yield List(sig, str).mkString

  def _unsignedDecIntLit: Gen[UnsignedDecimalIntegerLiteral] =
    _unsignedIntString("", 10).map(UnsignedDecimalIntegerLiteral(_)(pos))

  def _signedDecIntLit: Gen[SignedDecimalIntegerLiteral] =
    _signedIntString("", 10).map(SignedDecimalIntegerLiteral(_)(pos))

  def _signedHexIntLit: Gen[SignedHexIntegerLiteral] =
    _signedIntString("0x", 16).map(SignedHexIntegerLiteral(_)(pos))

  def _signedOctIntLitOldSyntax: Gen[SignedOctalIntegerLiteral] =
    _signedIntString("0", 8).map(SignedOctalIntegerLiteral(_)(pos))

  def _signedOctIntLit: Gen[SignedOctalIntegerLiteral] =
    _signedIntString("0o", 8).map(SignedOctalIntegerLiteral(_)(pos))

  def _signedIntLit: Gen[SignedIntegerLiteral] = oneOf(
    _signedDecIntLit,
    _signedHexIntLit,
    _signedOctIntLitOldSyntax,
    _signedOctIntLit
  )

  def _doubleLit: Gen[DecimalDoubleLiteral] =
    Arbitrary.arbDouble.arbitrary.map(_.toString).map(DecimalDoubleLiteral(_)(pos))

  def _parameter: Gen[Parameter] =
    _identifier.map(Parameter(_, AnyType.instance)(pos))

  def _stringParameter: Gen[Parameter] = _identifier.map(Parameter(_, CTString)(pos))

  def _mapParameter: Gen[Parameter] = _identifier.map(Parameter(_, CTMap)(pos))

  def _sensitiveStringParameter: Gen[Parameter with SensitiveParameter] =
    _identifier.map(new ExplicitParameter(_, CTString)(pos) with SensitiveParameter)

  def _sensitiveAutoStringParameter: Gen[Parameter with SensitiveAutoParameter] =
    _identifier.map(new ExplicitParameter(_, CTString)(pos) with SensitiveAutoParameter)

  def _variable: Gen[Variable] = {
    val nameGen = allowedVarNames match {
      case None        => _identifier
      case Some(Seq()) => const("").suchThat(_ => false)
      case Some(names) => oneOf(names)
    }
    for {
      name <- nameGen
    } yield Variable(name)(pos)
  }

  // Predicates
  // ----------------------------------

  def _predicateComparisonPar(l: Expression, r: Expression): Gen[Expression] = oneOf(
    GreaterThanOrEqual(l, r)(pos),
    GreaterThan(l, r)(pos),
    LessThanOrEqual(l, r)(pos),
    LessThan(l, r)(pos),
    Equals(l, r)(pos),
    NotEquals(l, r)(pos),
    InvalidNotEquals(l, r)(pos)
  )

  def _predicateComparison: Gen[Expression] = for {
    l <- _expression
    r <- _expression
    res <- _predicateComparisonPar(l, r)
  } yield res

  def _predicateComparisonChain: Gen[Expression] = for {
    exprs <- listOfN(4, _expression)
    pairs = exprs.sliding(2)
    gens = pairs.map(p => _predicateComparisonPar(p.head, p.last)).toList
    chain <- sequence(gens)(Buildable.buildableFactory)
  } yield Ands(chain)(pos)

  def _predicateUnary: Gen[Expression] = for {
    r <- _expression
    res <- oneOf(
      Not(r)(pos),
      IsNull(r)(pos),
      IsNotNull(r)(pos)
    )
  } yield res

  def _predicateBinary: Gen[Expression] = for {
    l <- _expression
    r <- _expression
    res <- oneOf(
      And(l, r)(pos),
      Or(l, r)(pos),
      Xor(l, r)(pos),
      RegexMatch(l, r)(pos),
      In(l, r)(pos),
      StartsWith(l, r)(pos),
      EndsWith(l, r)(pos),
      Contains(l, r)(pos)
    )
  } yield res

  // Collections
  // ----------------------------------

  def _map: Gen[MapExpression] = for {
    items <- zeroOrMore(tuple(_propertyKeyName, _expression))
  } yield MapExpression(items)(pos)

  def _mapStringKeys: Gen[Map[String, Expression]] = for {
    items <- zeroOrMore(tuple(_identifier, _expression))
  } yield items.toMap

  def _property: Gen[Property] = for {
    map <- _expression
    key <- _propertyKeyName
  } yield Property(map, key)(pos)

  def _mapProjectionElement: Gen[MapProjectionElement] =
    oneOf(
      for { key <- _propertyKeyName; exp <- _expression } yield LiteralEntry(key, exp)(pos),
      for { id <- _variable } yield VariableSelector(id)(pos),
      for { id <- _variable } yield PropertySelector(id)(pos),
      const(AllPropertiesSelector()(pos))
    )

  def _mapProjection: Gen[MapProjection] = for {
    name <- _variable
    items <- oneOrMore(_mapProjectionElement)
  } yield MapProjection(name, items)(pos)

  def _list: Gen[ListLiteral] =
    _listOf(_expression)

  def _listOf(expressionGen: Gen[Expression]): Gen[ListLiteral] = for {
    parts <- zeroOrMore(expressionGen)
  } yield ListLiteral(parts)(pos)

  def _listSlice: Gen[ListSlice] = for {
    list <- _expression
    from <- option(_expression)
    to <- option(_expression)
  } yield ListSlice(list, from, to)(pos)

  def _containerIndex: Gen[ContainerIndex] = for {
    expr <- _expression
    idx <- _expression
  } yield ContainerIndex(expr, idx)(pos)

  def _filterScope: Gen[FilterScope] = for {
    variable <- _variable
    innerPredicate <- option(_expression)
  } yield FilterScope(variable, innerPredicate)(pos)

  def _extractScope: Gen[ExtractScope] = for {
    variable <- _variable
    innerPredicate <- option(_expression)
    extractExpression <- option(_expression)
  } yield ExtractScope(variable, innerPredicate, extractExpression)(pos)

  def _listComprehension: Gen[ListComprehension] = for {
    scope <- _extractScope
    expression <- _expression
  } yield ListComprehension(scope, expression)(pos)

  def _iterablePredicate: Gen[IterablePredicateExpression] = for {
    scope <- _filterScope
    expression <- _expression
    predicate <- oneOf(
      AllIterablePredicate(scope, expression)(pos),
      AnyIterablePredicate(scope, expression)(pos),
      NoneIterablePredicate(scope, expression)(pos),
      SingleIterablePredicate(scope, expression)(pos)
    )
  } yield predicate

  def _reduceScope: Gen[ReduceScope] = for {
    accumulator <- _variable
    variable <- _variable
    expression <- _expression
  } yield ReduceScope(accumulator, variable, expression)(pos)

  def _reduceExpr: Gen[ReduceExpression] = for {
    scope <- _reduceScope
    init <- _expression
    list <- _expression
  } yield ReduceExpression(scope, init, list)(pos)

  // Arithmetic
  // ----------------------------------

  def _arithmeticUnary: Gen[Expression] = for {
    r <- _expression
    exp <- oneOf(
      UnaryAdd(r)(pos),
      UnarySubtract(r)(pos)
    )
  } yield exp

  def _arithmeticBinary: Gen[Expression] = for {
    l <- _expression
    r <- _expression
    exp <- oneOf(
      Add(l, r)(pos),
      Multiply(l, r)(pos),
      Divide(l, r)(pos),
      Pow(l, r)(pos),
      Modulo(l, r)(pos),
      Subtract(l, r)(pos)
    )
  } yield exp

  def _case: Gen[CaseExpression] = for {
    expression <- option(_expression)
    alternatives <- oneOrMore(tuple(_expression, _expression))
    default <- option(_expression)
  } yield CaseExpression(expression, alternatives, default)(pos)

  // Functions
  // ----------------------------------

  def _namespace: Gen[Namespace] = for {
    parts <- zeroOrMore(_identifier)
  } yield Namespace(parts)(pos)

  def _functionName: Gen[FunctionName] = for {
    name <- _identifier
  } yield FunctionName(name)(pos)

  def _functionInvocation: Gen[FunctionInvocation] = for {
    namespace <- _namespace
    functionName <- _functionName
    distinct <- boolean
    args <- zeroOrMore(_expression)
  } yield FunctionInvocation(namespace, functionName, distinct, args.toIndexedSeq)(pos)

  def _glob: Gen[String] = for {
    parts <- zeroOrMore(_identifier)
    name <- _identifier
  } yield parts.mkString(".") ++ name

  def _countStar: Gen[CountStar] =
    const(CountStar()(pos))

  // Patterns
  // ----------------------------------

  def _relationshipsPattern: Gen[RelationshipsPattern] = for {
    chain <- _relationshipChain
  } yield RelationshipsPattern(chain)(pos)

  def _patternExpr: Gen[PatternExpression] = for {
    pattern <- _relationshipsPattern
  } yield PatternExpression(pattern)(Set.empty)

  def _shortestPaths: Gen[ShortestPaths] = for {
    element <- _patternElement
    single <- boolean
  } yield ShortestPaths(element, single)(pos)

  def _shortestPathExpr: Gen[ShortestPathExpression] = for {
    pattern <- _shortestPaths
  } yield ShortestPathExpression(pattern)

  def _existsExpression: Gen[ExistsExpression] = for {
    pattern <- _pattern
    where <- option(_expression)
    outerScope <- zeroOrMore(_variable)
  } yield ExistsExpression(pattern, where)(pos, outerScope.toSet)

  def _countExpression: Gen[CountExpression] = for {
    pattern <- _pattern
    where <- option(_expression)
    outerScope <- zeroOrMore(_variable)
  } yield CountExpression(pattern, where)(pos, outerScope.toSet)

  def _patternComprehension: Gen[PatternComprehension] = for {
    namedPath <- option(_variable)
    pattern <- _relationshipsPattern
    predicate <- option(_expression)
    projection <- _expression
    outerScope <- zeroOrMore(_variable)
  } yield PatternComprehension(namedPath, pattern, predicate, projection)(pos, outerScope.toSet)

  // Expression
  // ----------------------------------

  def _expression: Gen[Expression] =
    frequency(
      5 -> oneOf(
        lzy(_nullLit),
        lzy(_stringLit),
        lzy(_booleanLit),
        lzy(_signedDecIntLit),
        lzy(_signedHexIntLit),
        lzy(_signedOctIntLitOldSyntax),
        lzy(_signedOctIntLit),
        lzy(_doubleLit),
        lzy(_variable),
        lzy(_parameter),
        lzy(_infinityLit),
        lzy(_nanLit)
      ),
      1 -> oneOf(
        lzy(_predicateComparison),
        lzy(_predicateUnary),
        lzy(_predicateBinary),
        lzy(_predicateComparisonChain),
        lzy(_iterablePredicate),
        lzy(_arithmeticUnary),
        lzy(_arithmeticBinary),
        lzy(_case),
        lzy(_functionInvocation),
        lzy(_countStar),
        lzy(_reduceExpr),
        lzy(_shortestPathExpr),
        lzy(_patternExpr),
        lzy(_map),
        lzy(_mapProjection),
        lzy(_property),
        lzy(_list),
        lzy(_listSlice),
        lzy(_listComprehension),
        lzy(_containerIndex),
        lzy(_existsExpression),
        lzy(_countExpression),
        lzy(_patternComprehension)
      )
    )

  def _labelExpression(entityType: Option[EntityType]): Gen[LabelExpression] = {

    def _labelExpressionConjunction: Gen[LabelExpression.Conjunctions] = for {
      lhs <- _labelExpression(entityType)
      rhs <- _labelExpression(entityType)
    } yield LabelExpression.Conjunctions.flat(lhs, rhs, pos)

    def _labelExpressionDisjunction: Gen[LabelExpression.Disjunctions] = for {
      lhs <- _labelExpression(entityType)
      rhs <- _labelExpression(entityType)
    } yield LabelExpression.Disjunctions.flat(lhs, rhs, pos)

    frequency(
      5 -> oneOf[LabelExpression](
        lzy(LabelExpression.Wildcard()(pos)),
        lzy(entityType match {
          case Some(NODE_TYPE)         => _labelName.map(Leaf(_))
          case Some(RELATIONSHIP_TYPE) => _relTypeName.map(Leaf(_))
          case None                    => _labelOrTypeName.map(Leaf(_))
        })
      ),
      1 -> oneOf[LabelExpression](
        lzy(_labelExpressionConjunction),
        lzy(_labelExpressionDisjunction),
        lzy(_labelExpression(entityType).map(LabelExpression.Negation(_)(pos)))
      )
    )
  }

  // PATTERNS
  // ==========================================================================

  def _nodePattern: Gen[NodePattern] = for {
    variable <- option(_variable)
    labelExpression <- option(_labelExpression(Some(NODE_TYPE)))
    properties <- option(oneOf(_map, _parameter))
    predicate <- variable match {
      case Some(_) => option(_expression) // Only generate WHERE if we have a variable name.
      case None    => const(None)
    }
  } yield NodePattern(variable, labelExpression, properties, predicate)(pos)

  def _range: Gen[Range] = for {
    lower <- option(_unsignedDecIntLit)
    upper <- option(_unsignedDecIntLit)
  } yield Range(lower, upper)(pos)

  def _semanticDirection: Gen[SemanticDirection] =
    oneOf(
      SemanticDirection.OUTGOING,
      SemanticDirection.INCOMING,
      SemanticDirection.BOTH
    )

  def _relationshipPattern: Gen[RelationshipPattern] = for {
    variable <- option(_variable)
    labelExpression <- option(_labelExpression(Some(RELATIONSHIP_TYPE)))
    length <- option(option(_range))
    properties <- option(oneOf(_map, _parameter))
    direction <- _semanticDirection
    predicate <- variable match {
      case None    => const(None)
      case Some(_) => option(_expression) // Only generate WHERE if we have a variable name.
    }
  } yield RelationshipPattern(variable, labelExpression, length, properties, predicate, direction)(pos)

  def _relationshipChain: Gen[RelationshipChain] = for {
    element <- _pathPrimary
    relationship <- _relationshipPattern
    rightNode <- _nodePattern
  } yield RelationshipChain(element, relationship, rightNode)(pos)

  def _pathPrimary: Gen[SimplePattern] = oneOf(
    _nodePattern,
    lzy(_relationshipChain)
  )

  def _generalQuantifier: Gen[IntervalQuantifier] = for {
    lower <- option(_unsignedDecIntLit)
    upper <- option(_unsignedDecIntLit)
  } yield IntervalQuantifier(lower, upper)(pos)

  def _fixedQuantifier: Gen[FixedQuantifier] = for {
    value <- _unsignedDecIntLit
  } yield FixedQuantifier(value)(pos)

  def _quantifier: Gen[GraphPatternQuantifier] = oneOf(
    const(StarQuantifier()(pos)),
    const(PlusQuantifier()(pos)),
    _generalQuantifier,
    _fixedQuantifier
  )

  def _quantifiedPath: Gen[QuantifiedPath] = for {
    primary <- _pathFactor
    quantifier <- _quantifier
  } yield QuantifiedPath(EveryPath(primary), quantifier)(pos)

  def _pathFactor: Gen[PathFactor] = oneOf(
    lzy(_quantifiedPath),
    lzy(_pathPrimary)
  )

  def _pathConcatenation: Gen[PathConcatenation] = for {
    elements <- twoOrMore(_pathFactor)
  } yield PathConcatenation(elements)(pos)

  def _patternElement: Gen[PatternElement] = oneOf(
    lzy(_pathFactor),
    lzy(_pathConcatenation)
  )

  def _anonPatternPart: Gen[AnonymousPatternPart] = for {
    element <- _patternElement
    single <- boolean
    part <- oneOf(
      EveryPath(element),
      ShortestPaths(element, single)(pos)
    )
  } yield part

  def _namedPatternPart: Gen[NamedPatternPart] = for {
    variable <- _variable
    part <- _anonPatternPart
  } yield NamedPatternPart(variable, part)(pos)

  def _patternPart: Gen[PatternPart] =
    oneOf(
      _anonPatternPart,
      _namedPatternPart
    )

  def _pattern: Gen[Pattern] = for {
    parts <- oneOrMore(_patternPart)
  } yield Pattern(parts)(pos)

  // CLAUSES
  // ==========================================================================

  def _returnItem: Gen[ReturnItem] = for {
    expr <- _expression
    variable <- _variable
    item <- oneOf(
      UnaliasedReturnItem(expr, "")(pos),
      AliasedReturnItem(expr, variable)(pos, isAutoAliased = false)
    )
  } yield item

  def _sortItem: Gen[SortItem] = for {
    expr <- _expression
    item <- oneOf(
      AscSortItem(expr)(pos),
      DescSortItem(expr)(pos)
    )
  } yield item

  def _orderBy: Gen[OrderBy] = for {
    items <- oneOrMore(_sortItem)
  } yield OrderBy(items)(pos)

  def _skip: Gen[Skip] =
    _expression.map(Skip(_)(pos))

  def _limit: Gen[Limit] =
    _expression.map(Limit(_)(pos))

  def _where: Gen[Where] =
    _expression.map(Where(_)(pos))

  def _returnItems1: Gen[ReturnItems] = for {
    retItems <- oneOrMore(_returnItem)
  } yield ReturnItems(includeExisting = false, retItems)(pos)

  def _returnItems2: Gen[ReturnItems] = for {
    retItems <- zeroOrMore(_returnItem)
  } yield ReturnItems(includeExisting = true, retItems)(pos)

  def _returnItems: Gen[ReturnItems] =
    oneOf(_returnItems1, _returnItems2)

  def _with: Gen[With] = for {
    distinct <- boolean
    inclExisting <- boolean
    retItems <- oneOrMore(_returnItem)
    orderBy <- option(_orderBy)
    skip <- option(_skip)
    limit <- option(_limit)
    where <- option(_where)
  } yield With(distinct, ReturnItems(inclExisting, retItems)(pos), orderBy, skip, limit, where)(pos)

  def _return: Gen[Return] = for {
    distinct <- boolean
    inclExisting <- boolean
    retItems <- oneOrMore(_returnItem)
    orderBy <- option(_orderBy)
    skip <- option(_skip)
    limit <- option(_limit)
  } yield Return(distinct, ReturnItems(inclExisting, retItems)(pos), orderBy, skip, limit)(pos)

  def _yield: Gen[Yield] = for {
    retItems <- oneOrMore(_yieldItem)
    orderBy <- option(_orderBy)
    skip <- option(_signedDecIntLit.map(Skip(_)(pos)))
    limit <- option(_signedDecIntLit.map(Limit(_)(pos)))
    where <- option(_where)
  } yield Yield(ReturnItems(includeExisting = false, retItems)(pos), orderBy, skip, limit, where)(pos)

  def _yieldItem: Gen[ReturnItem] = for {
    var1 <- _variable
    item <- UnaliasedReturnItem(var1, "")(pos)
  } yield item

  def _match: Gen[Match] = for {
    optional <- boolean
    pattern <- _pattern
    hints <- zeroOrMore(_hint)
    where <- option(_where)
  } yield Match(optional, pattern, hints, where)(pos)

  def _create: Gen[Create] = for {
    pattern <- _pattern
  } yield Create(pattern)(pos)

  def _unwind: Gen[Unwind] = for {
    expression <- _expression
    variable <- _variable
  } yield Unwind(expression, variable)(pos)

  def _setItem: Gen[SetItem] = for {
    variable <- _variable
    labels <- oneOrMore(_labelName)
    property <- _property
    expression <- _expression
    item <- oneOf(
      SetLabelItem(variable, labels)(pos),
      SetPropertyItem(property, expression)(pos),
      SetExactPropertiesFromMapItem(variable, expression)(pos),
      SetIncludingPropertiesFromMapItem(variable, expression)(pos)
    )
  } yield item

  def _removeItem: Gen[RemoveItem] = for {
    variable <- _variable
    labels <- oneOrMore(_labelName)
    property <- _property
    item <- oneOf(
      RemoveLabelItem(variable, labels)(pos),
      RemovePropertyItem(property)
    )
  } yield item

  def _set: Gen[SetClause] = for {
    items <- oneOrMore(_setItem)
  } yield SetClause(items)(pos)

  def _remove: Gen[Remove] = for {
    items <- oneOrMore(_removeItem)
  } yield Remove(items)(pos)

  def _delete: Gen[Delete] = for {
    expressions <- oneOrMore(_expression)
    forced <- boolean
  } yield Delete(expressions, forced)(pos)

  def _mergeAction: Gen[MergeAction] = for {
    set <- _set
    action <- oneOf(
      OnCreate(set)(pos),
      OnMatch(set)(pos)
    )
  } yield action

  def _merge: Gen[Merge] = for {
    pattern <- _patternPart
    actions <- oneOrMore(_mergeAction)
  } yield Merge(pattern, actions)(pos)

  def _procedureName: Gen[ProcedureName] = for {
    name <- _identifier
  } yield ProcedureName(name)(pos)

  def _procedureOutput: Gen[ProcedureOutput] = for {
    name <- _identifier
  } yield ProcedureOutput(name)(pos)

  def _procedureResultItem: Gen[ProcedureResultItem] = for {
    output <- option(_procedureOutput)
    variable <- _variable
  } yield ProcedureResultItem(output, variable)(pos)

  def _procedureResult: Gen[ProcedureResult] = for {
    items <- oneOrMore(_procedureResultItem)
    where <- option(_where)
  } yield ProcedureResult(items.toIndexedSeq, where)(pos)

  def _call: Gen[UnresolvedCall] = for {
    procedureNamespace <- _namespace
    procedureName <- _procedureName
    declaredArguments <- option(zeroOrMore(_expression))
    declaredResult <- option(_procedureResult)
    yieldAll <- if (declaredResult.isDefined) const(false) else boolean // can't have both YIELD * and declare results
  } yield UnresolvedCall(procedureNamespace, procedureName, declaredArguments, declaredResult, yieldAll)(pos)

  def _foreach: Gen[Foreach] = for {
    variable <- _variable
    expression <- _expression
    updates <- oneOrMore(_clause)
  } yield Foreach(variable, expression, updates)(pos)

  def _loadCsv: Gen[LoadCSV] = for {
    withHeaders <- boolean
    urlString <- _expression
    variable <- _variable
    fieldTerminator <- option(_stringLit)
  } yield LoadCSV(withHeaders, urlString, variable, fieldTerminator)(pos)

  // Hints
  // ----------------------------------

  def _usingIndexHint: Gen[UsingIndexHint] = for {
    variable <- _variable
    labelOrRelType <- _labelOrTypeName
    properties <- oneOrMore(_propertyKeyName)
    spec <- oneOf(SeekOnly, SeekOrScan)
    indexType <- oneOf(UsingAnyIndexType, UsingRangeIndexType, UsingTextIndexType, UsingPointIndexType)
  } yield UsingIndexHint(variable, labelOrRelType, properties, spec, indexType)(pos)

  def _usingJoinHint: Gen[UsingJoinHint] = for {
    variables <- oneOrMore(_variable)
  } yield UsingJoinHint(variables)(pos)

  def _usingScanHint: Gen[UsingScanHint] = for {
    variable <- _variable
    labelOrRelType <- _labelOrTypeName
  } yield UsingScanHint(variable, labelOrRelType)(pos)

  def _hint: Gen[UsingHint] = oneOf(
    _usingIndexHint,
    _usingJoinHint,
    _usingScanHint
  )

  // Queries
  // ----------------------------------

  def _use: Gen[UseGraph] = for {
    expression <- _expression
  } yield UseGraph(expression)(pos)

  def _subqueryCall: Gen[SubqueryCall] = for {
    part <- _queryPart
    params <- option(_inTransactionsParameters)
  } yield SubqueryCall(part, Nil, null, params)(pos)

  def _inTransactionsParameters: Gen[InTransactionsParameters] = for {
    batchSize <- option(_expression)
  } yield InTransactionsParameters(batchSize)(pos)

  def _clause: Gen[Clause] = oneOf(
    lzy(_use),
    lzy(_with),
    lzy(_return),
    lzy(_match),
    lzy(_create),
    lzy(_unwind),
    lzy(_set),
    lzy(_remove),
    lzy(_delete),
    lzy(_merge),
    lzy(_call),
    lzy(_foreach),
    lzy(_loadCsv),
    lzy(_subqueryCall)
  )

  def _singleQuery: Gen[SingleQuery] = for {
    s <- choose(1, 1)
    clauses <- listOfN(s, _clause)
  } yield SingleQuery(clauses)(pos)

  def _union: Gen[Union] = for {
    part <- _queryPart
    single <- _singleQuery
    union <- oneOf(
      UnionDistinct(part, single)(pos),
      UnionAll(part, single)(pos)
    )
  } yield union

  def _queryPart: Gen[QueryPart] = frequency(
    5 -> lzy(_singleQuery),
    1 -> lzy(_union)
  )

  def _query: Gen[Query] = for {
    part <- _queryPart
  } yield Query(part)(pos)

  // Show commands
  // ----------------------------------

  def _indexType: Gen[ShowIndexType] = for {
    indexType <- oneOf(AllIndexes, RangeIndexes, FulltextIndexes, TextIndexes, PointIndexes, LookupIndexes)
  } yield indexType

  def _listOfLabels: Gen[List[LabelName]] = for {
    labels <- oneOrMore(_labelName)
  } yield labels

  def _listOfRelTypes: Gen[List[RelTypeName]] = for {
    types <- oneOrMore(_relTypeName)
  } yield types

  def _constraintInfo: Gen[(ShowConstraintType, YieldOrWhere)] = for {
    unfilteredYields <- _eitherYieldOrWhere
    types <- oneOf(
      AllConstraints,
      UniqueConstraints,
      ExistsConstraints(ValidSyntax),
      NodeExistsConstraints(ValidSyntax),
      RelExistsConstraints(ValidSyntax),
      NodeKeyConstraints
    )
  } yield (types, unfilteredYields)

  def _showIndexes: Gen[Query] = for {
    indexType <- _indexType
    use <- option(_use)
    yields <- _eitherYieldOrWhere
  } yield {
    val showClauses = yields match {
      case Some(Right(w)) =>
        Seq(ShowIndexesClause(indexType, brief = false, verbose = false, Some(w), hasYield = false)(pos))
      case Some(Left((y, Some(r)))) =>
        Seq(ShowIndexesClause(indexType, brief = false, verbose = false, None, hasYield = true)(pos), y, r)
      case Some(Left((y, None))) =>
        Seq(ShowIndexesClause(indexType, brief = false, verbose = false, None, hasYield = true)(pos), y)
      case _ => Seq(ShowIndexesClause(indexType, brief = false, verbose = false, None, hasYield = false)(pos))
    }
    val fullClauses = use.map(u => u +: showClauses).getOrElse(showClauses)
    Query(SingleQuery(fullClauses)(pos))(pos)
  }

  def _showConstraints: Gen[Query] = for {
    (constraintType, yields) <- _constraintInfo
    use <- option(_use)
  } yield {
    val showClauses = yields match {
      case Some(Right(w)) =>
        Seq(ShowConstraintsClause(constraintType, brief = false, verbose = false, Some(w), hasYield = false)(pos))
      case Some(Left((y, Some(r)))) =>
        Seq(ShowConstraintsClause(constraintType, brief = false, verbose = false, None, hasYield = true)(pos), y, r)
      case Some(Left((y, None))) =>
        Seq(ShowConstraintsClause(constraintType, brief = false, verbose = false, None, hasYield = true)(pos), y)
      case _ => Seq(ShowConstraintsClause(constraintType, brief = false, verbose = false, None, hasYield = false)(pos))
    }
    val fullClauses = use.map(u => u +: showClauses).getOrElse(showClauses)
    Query(SingleQuery(fullClauses)(pos))(pos)
  }

  def _showProcedures: Gen[Query] = for {
    name <- _identifier
    exec <- option(oneOf(CurrentUser, User(name)))
    yields <- _eitherYieldOrWhere
    use <- option(_use)
  } yield {
    val showClauses = yields match {
      case Some(Right(w))           => Seq(ShowProceduresClause(exec, Some(w), hasYield = false)(pos))
      case Some(Left((y, Some(r)))) => Seq(ShowProceduresClause(exec, None, hasYield = true)(pos), y, r)
      case Some(Left((y, None)))    => Seq(ShowProceduresClause(exec, None, hasYield = true)(pos), y)
      case _                        => Seq(ShowProceduresClause(exec, None, hasYield = false)(pos))
    }
    val fullClauses = use.map(u => u +: showClauses).getOrElse(showClauses)
    Query(SingleQuery(fullClauses)(pos))(pos)
  }

  def _showFunctions: Gen[Query] = for {
    name <- _identifier
    funcType <- oneOf(AllFunctions, BuiltInFunctions, UserDefinedFunctions)
    exec <- option(oneOf(CurrentUser, User(name)))
    yields <- _eitherYieldOrWhere
    use <- option(_use)
  } yield {
    val showClauses = yields match {
      case Some(Right(w))           => Seq(ShowFunctionsClause(funcType, exec, Some(w), hasYield = false)(pos))
      case Some(Left((y, Some(r)))) => Seq(ShowFunctionsClause(funcType, exec, None, hasYield = true)(pos), y, r)
      case Some(Left((y, None)))    => Seq(ShowFunctionsClause(funcType, exec, None, hasYield = true)(pos), y)
      case _                        => Seq(ShowFunctionsClause(funcType, exec, None, hasYield = false)(pos))
    }
    val fullClauses = use.map(u => u +: showClauses).getOrElse(showClauses)
    Query(SingleQuery(fullClauses)(pos))(pos)
  }

  def _showTransactions: Gen[Query] = for {
    ids <- transactionIds
    yields <- _eitherYieldOrWhere
    yieldAll <- boolean
    use <- option(_use)
  } yield {
    val showClauses = yields match {
      case Some(Right(w)) => Seq(ShowTransactionsClause(ids, Some(w), List.empty, yieldAll = false)(pos))
      case Some(Left((y, Some(r)))) =>
        val (w, yi) = turnYieldToWith(y)
        Seq(ShowTransactionsClause(ids, None, yi, yieldAll = false)(pos), w, r)
      case Some(Left((y, None))) =>
        val (w, yi) = turnYieldToWith(y)
        Seq(ShowTransactionsClause(ids, None, yi, yieldAll = false)(pos), w)
      case _ if yieldAll =>
        Seq(ShowTransactionsClause(ids, None, List.empty, yieldAll = true)(pos), getFullWithStarFromYield)
      case _ => Seq(ShowTransactionsClause(ids, None, List.empty, yieldAll = false)(pos))
    }
    val fullClauses = use.map(u => u +: showClauses).getOrElse(showClauses)
    Query(SingleQuery(fullClauses)(pos))(pos)
  }

  def _terminateTransactions: Gen[Query] = for {
    ids <- transactionIds
    yields <- option(_yield)
    yieldAll <- boolean
    returns <- option(_return)
    use <- option(_use)
  } yield {
    val terminateClauses = (yields, returns) match {
      case (Some(y), Some(r)) =>
        val (w, yi) = turnYieldToWith(y)
        Seq(TerminateTransactionsClause(ids, yi, yieldAll = false, None)(pos), w, r)
      case (Some(y), None) =>
        val (w, yi) = turnYieldToWith(y)
        Seq(TerminateTransactionsClause(ids, yi, yieldAll = false, None)(pos), w)
      case _ if yieldAll =>
        Seq(TerminateTransactionsClause(ids, List.empty, yieldAll = true, None)(pos), getFullWithStarFromYield)
      case _ => Seq(TerminateTransactionsClause(ids, List.empty, yieldAll = false, None)(pos))
    }
    val fullClauses = use.map(u => u +: terminateClauses).getOrElse(terminateClauses)
    Query(SingleQuery(fullClauses)(pos))(pos)
  }

  def _combinedTransactionCommands: Gen[Query] = for {
    show <- showAsPartOfCombined
    terminate <- terminateAsPartOfCombined
    additionalShow <- zeroOrMore(showAsPartOfCombined)
    additionalTerminate <- zeroOrMore(terminateAsPartOfCombined)
    showFirst <- boolean
    returns <- _return
    use <- option(_use)
  } yield {

    val clauses =
      if (additionalShow.isEmpty && additionalTerminate.isEmpty) {
        // no additional clauses so take the two base ones
        if (showFirst) show ++ terminate else terminate ++ show
      } else if (additionalTerminate.isEmpty) {
        // Only additional show, make show only command
        // add base show to ensure at least 2 clauses
        show ++ additionalShow.flatten
      } else if (additionalShow.isEmpty) {
        // Only additional terminate, make terminate only command
        // add base terminate to ensure at least 2 clauses
        terminate ++ additionalTerminate.flatten
      } else {
        // multiple additional clauses, add all together and mix the order they appear in
        // (keeping the yield/with together with its respective clause)
        val allPairs = Seq(show, terminate) ++ additionalShow ++ additionalTerminate
        val scrambled = Random.shuffle(allPairs)
        scrambled.flatten
      }

    val clausesWithReturn = clauses :+ returns
    val fullClauses = use.map(u => u +: clausesWithReturn).getOrElse(clausesWithReturn)
    Query(SingleQuery(fullClauses)(pos))(pos)
  }

  private def showAsPartOfCombined: Gen[Seq[Clause]] = for {
    ids <- transactionIds
    yields <- _yield
    yieldAll <- boolean
  } yield {
    val (withClause, items) = turnYieldToWith(yields)
    if (yieldAll) Seq(ShowTransactionsClause(ids, None, List.empty, yieldAll = true)(pos), getFullWithStarFromYield)
    else Seq(ShowTransactionsClause(ids, None, items, yieldAll = false)(pos), withClause)
  }

  private def terminateAsPartOfCombined: Gen[Seq[Clause]] = for {
    ids <- transactionIds
    yields <- _yield
    yieldAll <- boolean
  } yield {
    val (withClause, items) = turnYieldToWith(yields)
    if (yieldAll)
      Seq(TerminateTransactionsClause(ids, List.empty, yieldAll = true, None)(pos), getFullWithStarFromYield)
    else Seq(TerminateTransactionsClause(ids, items, yieldAll = false, None)(pos), withClause)
  }

  /* Ids for the transaction commands:
   * - can be an expression or a list of strings
   * - a singular string is parsed as string expression
   * - no ids gives an empty list
   * - two or more ids give an id list
   */
  private def transactionIds: Gen[Either[List[String], Expression]] = for {
    multiIdList <- twoOrMore(string)
    idList <- oneOf(List.empty, multiIdList)
    expr <- _expression
    ids <- oneOf(Left(idList), Right(expr))
  } yield {
    ids
  }

  private def turnYieldToWith(yieldClause: Yield): (With, List[CommandResultItem]) = {
    val returnItems = yieldClause.returnItems
    val yieldItems =
      returnItems.items.map(r => {
        val variable = r.expression.asInstanceOf[Variable]
        val aliasedVariable = r.alias.getOrElse(variable)
        CommandResultItem(variable.name, aliasedVariable)(pos)
      }).toList
    val itemOrder = if (returnItems.items.nonEmpty) Some(returnItems.items.map(_.name).toList) else None
    val withClause = With(
      distinct = false,
      ReturnItems(includeExisting = true, Seq(), itemOrder)(returnItems.position),
      yieldClause.orderBy,
      yieldClause.skip,
      yieldClause.limit,
      yieldClause.where,
      withType = ParsedAsYield
    )(yieldClause.position)

    (withClause, yieldItems)
  }

  private def getFullWithStarFromYield =
    With(
      distinct = false,
      ReturnItems(includeExisting = true, Seq())(pos),
      None,
      None,
      None,
      None,
      withType = ParsedAsYield
    )(pos)

  def _showCommands: Gen[Query] = oneOf(
    _showIndexes,
    _showConstraints,
    _showProcedures,
    _showFunctions,
    _showTransactions,
    _terminateTransactions,
    _combinedTransactionCommands
  )

  // Schema commands
  // ----------------------------------

  def _variableProperty: Gen[Property] = for {
    map <- _variable
    key <- _propertyKeyName
  } yield Property(map, key)(pos)

  def _listOfProperties: Gen[List[Property]] = for {
    props <- oneOrMore(_variableProperty)
  } yield props

  def _createIndex: Gen[CreateIndex] = for {
    variable <- _variable
    labelName <- _labelName
    labels <- _listOfLabels
    relType <- _relTypeName
    types <- _listOfRelTypes
    props <- _listOfProperties
    name <- option(_identifier)
    ifExistsDo <- _ifExistsDo
    options <- _optionsMapAsEither
    fromDefault <- boolean
    use <- option(_use)
    rangeNodeIndex = CreateRangeNodeIndex(variable, labelName, props, name, ifExistsDo, options, fromDefault, use)(pos)
    rangeRelIndex =
      CreateRangeRelationshipIndex(variable, relType, props, name, ifExistsDo, options, fromDefault, use)(pos)
    lookupNodeIndex = CreateLookupIndex(
      variable,
      isNodeIndex = true,
      FunctionInvocation(FunctionName(Labels.name)(pos), distinct = false, IndexedSeq(variable))(pos),
      name,
      ifExistsDo,
      options,
      use
    )(pos)
    lookupRelIndex = CreateLookupIndex(
      variable,
      isNodeIndex = false,
      FunctionInvocation(FunctionName(Type.name)(pos), distinct = false, IndexedSeq(variable))(pos),
      name,
      ifExistsDo,
      options,
      use
    )(pos)
    fulltextNodeIndex = CreateFulltextNodeIndex(variable, labels, props, name, ifExistsDo, options, use)(pos)
    fulltextRelIndex = CreateFulltextRelationshipIndex(variable, types, props, name, ifExistsDo, options, use)(pos)
    textNodeIndex = CreateTextNodeIndex(variable, labelName, props, name, ifExistsDo, options, use)(pos)
    textRelIndex = CreateTextRelationshipIndex(variable, relType, props, name, ifExistsDo, options, use)(pos)
    pointNodeIndex = CreatePointNodeIndex(variable, labelName, props, name, ifExistsDo, options, use)(pos)
    pointRelIndex = CreatePointRelationshipIndex(variable, relType, props, name, ifExistsDo, options, use)(pos)
    command <- oneOf(
      rangeNodeIndex,
      rangeRelIndex,
      lookupNodeIndex,
      lookupRelIndex,
      fulltextNodeIndex,
      fulltextRelIndex,
      textNodeIndex,
      textRelIndex,
      pointNodeIndex,
      pointRelIndex
    )
  } yield command

  def _dropIndex: Gen[DropIndexOnName] = for {
    name <- _identifier
    ifExists <- boolean
    use <- option(_use)
  } yield DropIndexOnName(name, ifExists, use)(pos)

  def _createConstraint: Gen[SchemaCommand] = for {
    variable <- _variable
    labelName <- _labelName
    relTypeName <- _relTypeName
    props <- _listOfProperties
    prop <- _variableProperty
    name <- option(_identifier)
    ifExistsDo <- _ifExistsDo
    containsOn <- boolean
    options <- _optionsMapAsEither
    use <- option(_use)
    nodeKey = CreateNodeKeyConstraint(
      variable,
      labelName,
      props,
      name,
      ifExistsDo,
      options,
      containsOn,
      ConstraintVersion2,
      use
    )(pos)
    uniqueness = CreateUniquePropertyConstraint(
      variable,
      labelName,
      Seq(prop),
      name,
      ifExistsDo,
      options,
      containsOn,
      ConstraintVersion2,
      use
    )(pos)
    compositeUniqueness = CreateUniquePropertyConstraint(
      variable,
      labelName,
      props,
      name,
      ifExistsDo,
      options,
      containsOn,
      ConstraintVersion2,
      use
    )(pos)
    nodeExistence = CreateNodePropertyExistenceConstraint(
      variable,
      labelName,
      prop,
      name,
      ifExistsDo,
      options,
      containsOn,
      ConstraintVersion2,
      use
    )(pos)
    relExistence = CreateRelationshipPropertyExistenceConstraint(
      variable,
      relTypeName,
      prop,
      name,
      ifExistsDo,
      options,
      containsOn,
      ConstraintVersion2,
      use
    )(pos)
    command <- oneOf(nodeKey, uniqueness, compositeUniqueness, nodeExistence, relExistence)
  } yield command

  def _dropConstraint: Gen[DropConstraintOnName] = for {
    name <- _identifier
    ifExists <- boolean
    use <- option(_use)
  } yield DropConstraintOnName(name, ifExists, use)(pos)

  def _indexCommand: Gen[SchemaCommand] = oneOf(_createIndex, _dropIndex)

  def _constraintCommand: Gen[SchemaCommand] = oneOf(_createConstraint, _dropConstraint)

  def _schemaCommand: Gen[SchemaCommand] = oneOf(_indexCommand, _constraintCommand)

  // Administration commands
  // ----------------------------------

  def _nameAsEither: Gen[Either[String, Parameter]] = for {
    name <- _identifier
    param <- _stringParameter
    finalName <- oneOf(Left(name), Right(param))
  } yield finalName

  def _databaseName: Gen[DatabaseName] = for {
    namespacedName <- _namespacedName
    param <- _stringParameter
    finalName <- oneOf(namespacedName, ParameterName(param)(pos))
  } yield finalName

  def _databaseNameNoNamespace: Gen[DatabaseName] = for {
    name <- listOfN(1, _identifier)
    param <- _stringParameter
    finalName <- oneOf(NamespacedName(name)(pos), ParameterName(param)(pos))
  } yield finalName

  def _optionsMapAsEither: Gen[Options] = for {
    map <- oneOrMore(tuple(_identifier, _expression)).map(_.toMap)
    param <- _mapParameter
    finalMap <- oneOf(OptionsMap(map), OptionsParam(param), NoOptions)
  } yield finalMap

  def _optionalMapAsEither: Gen[Either[Map[String, Expression], Parameter]] = for {
    map <- oneOrMore(tuple(_identifier, _expression)).map(_.toMap)
    param <- _mapParameter
    finalMap <- oneOf(Left(map), Right(param))
  } yield finalMap

  def _listOfNameOfEither: Gen[List[Either[String, Parameter]]] = for {
    names <- oneOrMore(_nameAsEither)
  } yield names

  def _password: Gen[Expression] =
    oneOf(_sensitiveStringParameter, _sensitiveAutoStringParameter, _sensitiveStringLiteral)

  def _ifExistsDo: Gen[IfExistsDo] =
    oneOf(IfExistsReplace, IfExistsDoNothing, IfExistsThrowError, IfExistsInvalidSyntax)

  def _namespacedName: Gen[NamespacedName] = for {
    name <- listOfN(1, _identifier)
    namespace <- _identifier
    maybeNamespace <- option(namespace)
  } yield NamespacedName(name, maybeNamespace)(pos)

  // User commands

  def _showUsers: Gen[ShowUsers] = for {
    yields <- _eitherYieldOrWhere
  } yield ShowUsers(yields)(pos)

  def _showCurrentUser: Gen[ShowCurrentUser] = for {
    yields <- _eitherYieldOrWhere
  } yield ShowCurrentUser(yields)(pos)

  def _eitherYieldOrWhere: Gen[YieldOrWhere] = for {
    yields <- _yield
    where <- _where
    returns <- option(_return)
    eyw <- oneOf(Seq(Left((yields, returns)), Right(where)))
    oeyw <- option(eyw)
  } yield oeyw

  def _createUser: Gen[CreateUser] = for {
    userName <- _nameAsEither
    isEncryptedPassword <- boolean
    password <- _password
    requirePasswordChange <- boolean
    suspended <- option(boolean)
    homeDatabase <- option(_setHomeDatabaseAction)
    ifExistsDo <- _ifExistsDo
    // requirePasswordChange is parsed as 'Some(true)' if omitted in query,
    // prettifier explicitly adds it so 'None' would be prettified and re-parsed to 'Some(true)'
    // hence the explicit 'Some(requirePasswordChange)'
  } yield CreateUser(userName, isEncryptedPassword, password, UserOptions(Some(requirePasswordChange), suspended, homeDatabase), ifExistsDo)(pos)

  def _renameUser: Gen[RenameUser] = for {
    fromUserName <- _nameAsEither
    toUserName <- _nameAsEither
    ifExists <- boolean
  } yield RenameUser(fromUserName, toUserName, ifExists)(pos)

  def _dropUser: Gen[DropUser] = for {
    userName <- _nameAsEither
    ifExists <- boolean
  } yield DropUser(userName, ifExists)(pos)

  def _alterUser: Gen[AlterUser] = for {
    userName <- _nameAsEither
    ifExists <- boolean
    password <- option(_password)
    requirePasswordChange <- option(boolean)
    isEncryptedPassword <- if (password.isEmpty) const(None) else some(boolean)
    suspended <- option(boolean)
    // All four are not allowed to be None and REMOVE HOME DATABASE is only valid by itself
    homeDatabase <-
      if (password.isEmpty && requirePasswordChange.isEmpty && suspended.isEmpty)
        oneOf(some(_setHomeDatabaseAction), some(RemoveHomeDatabaseAction))
      else option(_setHomeDatabaseAction)
  } yield AlterUser(userName, isEncryptedPassword, password, UserOptions(requirePasswordChange, suspended, homeDatabase), ifExists)(pos)

  def _setHomeDatabaseAction: Gen[SetHomeDatabaseAction] = _databaseName.map(db => SetHomeDatabaseAction(db))

  def _setOwnPassword: Gen[SetOwnPassword] = for {
    newPassword <- _password
    oldPassword <- _password
  } yield SetOwnPassword(newPassword, oldPassword)(pos)

  def _userCommand: Gen[AdministrationCommand] = oneOf(
    _showUsers,
    _showCurrentUser,
    _createUser,
    _renameUser,
    _dropUser,
    _alterUser,
    _setOwnPassword
  )

  // Role commands

  def _showRoles: Gen[ShowRoles] = for {
    withUsers <- boolean
    showAll <- boolean
    yields <- _eitherYieldOrWhere
  } yield ShowRoles(withUsers, showAll, yields)(pos)

  def _createRole: Gen[CreateRole] = for {
    roleName <- _nameAsEither
    fromRoleName <- option(_nameAsEither)
    ifExistsDo <- _ifExistsDo
  } yield CreateRole(roleName, fromRoleName, ifExistsDo)(pos)

  def _renameRole: Gen[RenameRole] = for {
    fromRoleName <- _nameAsEither
    toRoleName <- _nameAsEither
    ifExists <- boolean
  } yield RenameRole(fromRoleName, toRoleName, ifExists)(pos)

  def _dropRole: Gen[DropRole] = for {
    roleName <- _nameAsEither
    ifExists <- boolean
  } yield DropRole(roleName, ifExists)(pos)

  def _grantRole: Gen[GrantRolesToUsers] = for {
    roleNames <- _listOfNameOfEither
    userNames <- _listOfNameOfEither
  } yield GrantRolesToUsers(roleNames, userNames)(pos)

  def _revokeRole: Gen[RevokeRolesFromUsers] = for {
    roleNames <- _listOfNameOfEither
    userNames <- _listOfNameOfEither
  } yield RevokeRolesFromUsers(roleNames, userNames)(pos)

  def _roleCommand: Gen[AdministrationCommand] = oneOf(
    _showRoles,
    _createRole,
    _renameRole,
    _dropRole,
    _grantRole,
    _revokeRole
  )

  // Privilege commands

  def _revokeType: Gen[RevokeType] = oneOf(RevokeGrantType()(pos), RevokeDenyType()(pos), RevokeBothType()(pos))

  def _dbmsAction: Gen[DbmsAction] = oneOf(
    AllDbmsAction,
    ExecuteProcedureAction,
    ExecuteBoostedProcedureAction,
    ExecuteAdminProcedureAction,
    ExecuteFunctionAction,
    ExecuteBoostedFunctionAction,
    ImpersonateUserAction,
    AllUserActions,
    ShowUserAction,
    CreateUserAction,
    RenameUserAction,
    SetUserStatusAction,
    SetUserHomeDatabaseAction,
    SetPasswordsAction,
    AlterUserAction,
    DropUserAction,
    AllRoleActions,
    ShowRoleAction,
    CreateRoleAction,
    RenameRoleAction,
    DropRoleAction,
    AssignRoleAction,
    RemoveRoleAction,
    AllDatabaseManagementActions,
    CreateDatabaseAction,
    CreateCompositeDatabaseAction,
    DropDatabaseAction,
    DropCompositeDatabaseAction,
    CompositeDatabaseManagementActions,
    AlterDatabaseAction,
    SetDatabaseAccessAction,
    AllAliasManagementActions,
    CreateAliasAction,
    DropAliasAction,
    AlterAliasAction,
    ShowAliasAction,
    AllPrivilegeActions,
    ShowPrivilegeAction,
    AssignPrivilegeAction,
    RemovePrivilegeAction,
    ServerManagementAction,
    ShowServerAction
  )

  def _databaseAction: Gen[DatabaseAction] = oneOf(
    StartDatabaseAction,
    StopDatabaseAction,
    AllDatabaseAction,
    AccessDatabaseAction,
    AllIndexActions,
    CreateIndexAction,
    DropIndexAction,
    ShowIndexAction,
    AllConstraintActions,
    CreateConstraintAction,
    DropConstraintAction,
    ShowConstraintAction,
    AllTokenActions,
    CreateNodeLabelAction,
    CreateRelationshipTypeAction,
    CreatePropertyKeyAction,
    AllTransactionActions,
    ShowTransactionAction,
    TerminateTransactionAction
  )

  def _graphAction: Gen[GraphAction] = oneOf(
    TraverseAction,
    ReadAction,
    MatchAction,
    MergeAdminAction,
    CreateElementAction,
    DeleteElementAction,
    WriteAction,
    RemoveLabelAction,
    SetLabelAction,
    SetPropertyAction,
    AllGraphAction
  )

  def _dbmsQualifier(dbmsAction: DbmsAction): Gen[List[PrivilegeQualifier]] =
    if (dbmsAction == ExecuteProcedureAction || dbmsAction == ExecuteBoostedProcedureAction) {
      // Procedures
      for {
        glob <- _glob
        procedures <- oneOrMore(ProcedureQualifier(glob)(pos))
        qualifier <- frequency(7 -> procedures, 3 -> List(ProcedureQualifier("*")(pos)))
      } yield qualifier
    } else if (dbmsAction == ExecuteFunctionAction || dbmsAction == ExecuteBoostedFunctionAction) {
      // Functions
      for {
        glob <- _glob
        functions <- oneOrMore(FunctionQualifier(glob)(pos))
        qualifier <- frequency(7 -> functions, 3 -> List(FunctionQualifier("*")(pos)))
      } yield qualifier
    } else if (dbmsAction == ImpersonateUserAction) {
      // impersonation
      for {
        userNames <- _listOfNameOfEither
        qualifier <- frequency(7 -> userNames.map(UserQualifier(_)(pos)), 3 -> List(UserAllQualifier()(pos)))
      } yield qualifier
    } else {
      // All other dbms privileges have AllQualifier
      List(AllQualifier()(pos))
    }

  def _databaseQualifier(haveUserQualifier: Boolean): Gen[List[DatabasePrivilegeQualifier]] =
    if (haveUserQualifier) {
      for {
        userNames <- _listOfNameOfEither
        qualifier <- frequency(7 -> userNames.map(UserQualifier(_)(pos)), 3 -> List(UserAllQualifier()(pos)))
      } yield qualifier
    } else {
      List(AllDatabasesQualifier()(pos))
    }

  def _graphQualifier: Gen[List[GraphPrivilegeQualifier]] = for {
    qualifierNames <- oneOrMore(_identifier)
    qualifier <- oneOf(
      qualifierNames.map(RelationshipQualifier(_)(pos)),
      List(RelationshipAllQualifier()(pos)),
      qualifierNames.map(LabelQualifier(_)(pos)),
      List(LabelAllQualifier()(pos)),
      qualifierNames.map(ElementQualifier(_)(pos)),
      List(ElementsAllQualifier()(pos))
    )
  } yield qualifier

  def _graphQualifierAndResource(graphAction: GraphAction)
    : Gen[(List[GraphPrivilegeQualifier], Option[ActionResource])] =
    if (graphAction == AllGraphAction) {
      // ALL GRAPH PRIVILEGES has AllQualifier and no resource
      (List(AllQualifier()(pos)), None)
    } else if (graphAction == WriteAction) {
      // WRITE has AllElementsQualifier and no resource
      (List(ElementsAllQualifier()(pos)), None)
    } else if (graphAction == SetLabelAction || graphAction == RemoveLabelAction) {
      // SET/REMOVE LABEL have AllLabelQualifier and label resource
      for {
        resourceNames <- oneOrMore(_identifier)
        resource <- oneOf(LabelsResource(resourceNames)(pos), AllLabelResource()(pos))
      } yield (List(LabelAllQualifier()(pos)), Some(resource))
    } else if (
      graphAction == TraverseAction || graphAction == CreateElementAction || graphAction == DeleteElementAction
    ) {
      // TRAVERSE, CREATE/DELETE ELEMENT have any graph qualifier and no resource
      for {
        qualifier <- _graphQualifier
      } yield (qualifier, None)
    } else {
      // READ, MATCH, MERGE, SET PROPERTY have any graph qualifier and property resource
      for {
        qualifier <- _graphQualifier
        resourceNames <- oneOrMore(_identifier)
        resource <- oneOf(PropertiesResource(resourceNames)(pos), AllPropertyResource()(pos))
      } yield (qualifier, Some(resource))
    }

  def _showPrivileges: Gen[ShowPrivileges] = for {
    names <- _listOfNameOfEither
    showRole = ShowRolesPrivileges(names)(pos)
    showUser1 = ShowUsersPrivileges(names)(pos)
    showUser2 = ShowUserPrivileges(None)(pos)
    showAll = ShowAllPrivileges()(pos)
    scope <- oneOf(showRole, showUser1, showUser2, showAll)
    yields <- _eitherYieldOrWhere
  } yield ShowPrivileges(scope, yields)(pos)

  def _showPrivilegeCommands: Gen[ShowPrivilegeCommands] = for {
    names <- _listOfNameOfEither
    showRole = ShowRolesPrivileges(names)(pos)
    showUser1 = ShowUsersPrivileges(names)(pos)
    showUser2 = ShowUserPrivileges(None)(pos)
    showAll = ShowAllPrivileges()(pos)
    scope <- oneOf(showRole, showUser1, showUser2, showAll)
    asRevoke <- boolean
    yields <- _eitherYieldOrWhere
  } yield ShowPrivilegeCommands(scope, asRevoke, yields)(pos)

  def _dbmsPrivilege: Gen[PrivilegeCommand] = for {
    dbmsAction <- _dbmsAction
    qualifier <- _dbmsQualifier(dbmsAction)
    roleNames <- _listOfNameOfEither
    revokeType <- _revokeType
    immutable <- boolean
    dbmsGrant = GrantPrivilege.dbmsAction(dbmsAction, immutable, roleNames, qualifier)(pos)
    dbmsDeny = DenyPrivilege.dbmsAction(dbmsAction, immutable, roleNames, qualifier)(pos)
    dbmsRevoke = RevokePrivilege.dbmsAction(dbmsAction, immutable, roleNames, revokeType, qualifier)(pos)
    dbms <- oneOf(dbmsGrant, dbmsDeny, dbmsRevoke)
  } yield dbms

  def _databasePrivilege: Gen[PrivilegeCommand] = for {
    databaseAction <- _databaseAction
    namedScope <- oneOrMore(_databaseName).map(_.map(n => NamedDatabaseScope(n)(pos)))
    databaseScope <- oneOf(
      namedScope,
      List(AllDatabasesScope()(pos)),
      List(DefaultDatabaseScope()(pos)),
      List(HomeDatabaseScope()(pos))
    )
    databaseQualifier <- _databaseQualifier(databaseAction.isInstanceOf[TransactionManagementAction])
    roleNames <- _listOfNameOfEither
    revokeType <- _revokeType
    immutable <- boolean
    databaseGrant =
      GrantPrivilege.databaseAction(databaseAction, immutable, databaseScope, roleNames, databaseQualifier)(pos)
    databaseDeny =
      DenyPrivilege.databaseAction(databaseAction, immutable, databaseScope, roleNames, databaseQualifier)(pos)
    databaseRevoke =
      RevokePrivilege.databaseAction(
        databaseAction,
        immutable,
        databaseScope,
        roleNames,
        revokeType,
        databaseQualifier
      )(pos)
    database <- oneOf(databaseGrant, databaseDeny, databaseRevoke)
  } yield database

  def _graphPrivilege: Gen[PrivilegeCommand] = for {
    graphAction <- _graphAction
    namedScope <- oneOrMore(_databaseName).map(_.map(n => NamedGraphScope(n)(pos)))
    graphScope <-
      oneOf(namedScope, List(AllGraphsScope()(pos)), List(DefaultGraphScope()(pos)), List(HomeGraphScope()(pos)))
    (qualifier, maybeResource) <- _graphQualifierAndResource(graphAction)
    roleNames <- _listOfNameOfEither
    revokeType <- _revokeType
    immutable <- boolean
    graphGrant =
      GrantPrivilege.graphAction(graphAction, immutable, maybeResource, graphScope, qualifier, roleNames)(pos)
    graphDeny = DenyPrivilege.graphAction(graphAction, immutable, maybeResource, graphScope, qualifier, roleNames)(pos)
    graphRevoke =
      RevokePrivilege.graphAction(graphAction, immutable, maybeResource, graphScope, qualifier, roleNames, revokeType)(
        pos
      )
    graph <- oneOf(graphGrant, graphDeny, graphRevoke)
  } yield graph

  def _privilegeCommand: Gen[AdministrationCommand] = oneOf(
    _showPrivileges,
    _showPrivilegeCommands,
    _dbmsPrivilege,
    _databasePrivilege,
    _graphPrivilege
  )

  // Database commands

  def _showDatabase: Gen[ShowDatabase] = for {
    dbName <- _databaseName
    scope <- oneOf(
      NamedDatabaseScope(dbName)(pos),
      AllDatabasesScope()(pos),
      DefaultDatabaseScope()(pos),
      HomeDatabaseScope()(pos)
    )
    yields <- _eitherYieldOrWhere
  } yield ShowDatabase(scope, yields)(pos)

  def _createDatabase: Gen[CreateDatabase] = for {
    dbName <- _databaseNameNoNamespace
    ifExistsDo <- _ifExistsDo
    wait <- _waitUntilComplete
    options <- _optionsMapAsEither
  } yield CreateDatabase(dbName, ifExistsDo, options, wait)(pos)

  def _createCompositeDatabase: Gen[CreateCompositeDatabase] = for {
    dbName <- _databaseName
    ifExistsDo <- _ifExistsDo
    wait <- _waitUntilComplete
  } yield CreateCompositeDatabase(dbName, ifExistsDo, wait)(pos)

  def _dropDatabase: Gen[DropDatabase] = for {
    dbName <- _databaseName
    ifExists <- boolean
    composite <- boolean
    additionalAction <- Gen.oneOf(DumpData, DestroyData)
    wait <- _waitUntilComplete
  } yield DropDatabase(dbName, ifExists, composite, additionalAction, wait)(pos)

  def _alterDatabase: Gen[AlterDatabase] = for {
    dbName <- _databaseName
    ifExists <- boolean
    access <- _access
  } yield AlterDatabase(dbName, ifExists, access)(pos)

  def _startDatabase: Gen[StartDatabase] = for {
    dbName <- _databaseName
    wait <- _waitUntilComplete
  } yield StartDatabase(dbName, wait)(pos)

  def _stopDatabase: Gen[StopDatabase] = for {
    dbName <- _databaseName
    wait <- _waitUntilComplete
  } yield StopDatabase(dbName, wait)(pos)

  def _multiDatabaseCommand: Gen[AdministrationCommand] = oneOf(
    _showDatabase,
    _createDatabase,
    _createCompositeDatabase,
    _dropDatabase,
    _alterDatabase,
    _startDatabase,
    _stopDatabase
  )

  def _access: Gen[Access] = for {
    access <- oneOf(ReadOnlyAccess, ReadWriteAccess)
  } yield access

  def _waitUntilComplete: Gen[WaitUntilComplete] = for {
    timeout <- posNum[Long]
    wait <- oneOf(NoWait, IndefiniteWait, TimeoutAfter(timeout))
  } yield wait

  def _createLocalDatabaseAlias: Gen[CreateLocalDatabaseAlias] = for {
    aliasName <- _databaseName
    targetName <- _databaseName
    ifExistsDo <- _ifExistsDo
    properties <- option(_optionalMapAsEither)
  } yield CreateLocalDatabaseAlias(aliasName, targetName, ifExistsDo, properties)(pos)

  def _createRemoteDatabaseAlias: Gen[CreateRemoteDatabaseAlias] = for {
    aliasName <- _databaseName
    targetName <- _databaseName
    ifExistsDo <- _ifExistsDo
    url <- _nameAsEither
    username <- _nameAsEither
    password <- _password
    driverSettings <- option(_optionalMapAsEither)
    properties <- option(_optionalMapAsEither)
  } yield CreateRemoteDatabaseAlias(aliasName, targetName, ifExistsDo, url, username, password, driverSettings, properties)(pos)

  def _dropAlias: Gen[DropDatabaseAlias] = for {
    aliasName <- _databaseName
    ifExists <- boolean
  } yield DropDatabaseAlias(aliasName, ifExists)(pos)

  def _alterLocalAlias: Gen[AlterLocalDatabaseAlias] = for {
    aliasName <- _databaseName
    targetName <- option(_databaseName)
    ifExists <- boolean
    properties <-
      if (targetName.isEmpty) some(_optionalMapAsEither)
      else option(_optionalMapAsEither)
  } yield AlterLocalDatabaseAlias(aliasName, targetName, ifExists, properties)(pos)

  def _alterRemoteAlias: Gen[AlterRemoteDatabaseAlias] = for {
    aliasName <- _databaseName
    targetName <- option(_databaseName)
    ifExists <- boolean
    url <- if (targetName.nonEmpty) some(_nameAsEither) else const(None)
    username <- option(_nameAsEither)
    password <- option(_password)
    // All four are not allowed to be None
    driverSettings <-
      if (url.isEmpty && username.isEmpty && password.isEmpty)
        some(_optionalMapAsEither)
      else
        option(_optionalMapAsEither)
    properties <- option(_optionalMapAsEither)
  } yield AlterRemoteDatabaseAlias(aliasName, targetName, ifExists, url, username, password, driverSettings, properties)(pos)

  def _showAliases: Gen[ShowAliases] = for {
    dbName <- option(_databaseName)
    yields <- _eitherYieldOrWhere
  } yield ShowAliases(dbName, yields)(pos)

  def _aliasCommands: Gen[AdministrationCommand] = oneOf(
    _createLocalDatabaseAlias,
    _createRemoteDatabaseAlias,
    _dropAlias,
    _alterLocalAlias,
    _alterRemoteAlias,
    _showAliases
  )

  // Server commands

  def _serverCommand: Gen[AdministrationCommand] = oneOf(
    _enableServer,
    _renameServer,
    _dropServer,
    _deallocateServer,
    _showServers
  )

  def _enableServer: Gen[EnableServer] = for {
    serverName <- _nameAsEither
    options <- _optionsMapAsEither
  } yield EnableServer(serverName, options)(pos)

  def _renameServer: Gen[RenameServer] = for {
    serverName <- _nameAsEither
    newName <- _nameAsEither
  } yield RenameServer(serverName, newName)(pos)

  def _showServers: Gen[ShowServers] = for {
    yields <- _eitherYieldOrWhere
  } yield ShowServers(yields)(pos)

  def _dropServer: Gen[DropServer] = for {
    serverName <- _nameAsEither
  } yield DropServer(serverName)(pos)

  def _deallocateServer: Gen[DeallocateServers] = for {
    servers <- _listOfNameOfEither
  } yield DeallocateServers(servers)(pos)

  // Top level administration command

  def _adminCommand: Gen[AdministrationCommand] = for {
    command <-
      oneOf(_userCommand, _roleCommand, _privilegeCommand, _multiDatabaseCommand, _aliasCommands, _serverCommand)
    use <- frequency(1 -> some(_use), 9 -> const(None))
  } yield command.withGraph(use)

  // Top level statement
  // ----------------------------------

  def _statement: Gen[Statement] = oneOf(
    _query,
    _schemaCommand,
    _showCommands,
    _adminCommand
  )
}
