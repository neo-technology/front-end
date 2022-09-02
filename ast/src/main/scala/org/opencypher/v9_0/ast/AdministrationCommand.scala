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
package org.opencypher.v9_0.ast

import org.opencypher.v9_0.ast.prettifier.Prettifier
import org.opencypher.v9_0.ast.semantics.SemanticAnalysisTooling
import org.opencypher.v9_0.ast.semantics.SemanticCheck
import org.opencypher.v9_0.ast.semantics.SemanticCheck.success
import org.opencypher.v9_0.ast.semantics.SemanticCheck.when
import org.opencypher.v9_0.ast.semantics.SemanticCheckResult
import org.opencypher.v9_0.ast.semantics.SemanticFeature
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.CountExpression
import org.opencypher.v9_0.expressions.ExistsExpression
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.Parameter
import org.opencypher.v9_0.expressions.PatternComprehension
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.SubqueryExpression
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.CTBoolean
import org.opencypher.v9_0.util.symbols.CTDateTime
import org.opencypher.v9_0.util.symbols.CTInteger
import org.opencypher.v9_0.util.symbols.CTList
import org.opencypher.v9_0.util.symbols.CTMap
import org.opencypher.v9_0.util.symbols.CTString

import java.util

import scala.jdk.CollectionConverters.ListHasAsScala

sealed trait AdministrationCommand extends StatementWithGraph with SemanticAnalysisTooling {

  def name: String

  // We parse USE to give a nice error message, but it's not considered to be a part of the AST
  private var useGraphVar: Option[UseGraph] = None
  def useGraph: Option[UseGraph] = useGraphVar

  override def withGraph(useGraph: Option[UseGraph]): AdministrationCommand = {
    this.useGraphVar = useGraph
    this
  }

  def isReadOnly: Boolean

  override def containsUpdates: Boolean = !isReadOnly

  override def semanticCheck: SemanticCheck =
    requireFeatureSupport(s"The `$name` clause", SemanticFeature.MultipleDatabases, position) chain
      when(useGraphVar.isDefined)(error(
        s"The `USE` clause is not required for Administration Commands. Retry your query omitting the `USE` clause and it will be routed automatically.",
        position
      ))
}

sealed trait ReadAdministrationCommand extends AdministrationCommand {

  val isReadOnly: Boolean = true

  private[ast] val defaultColumnSet: List[ShowColumn]

  def returnColumnNames: List[String] = (yields, returns) match {
    case (_, Some(r))                => r.returnItems.items.map(ri => ri.alias.get.name).toList
    case (Some(resultColumns), None) => resultColumns.returnItems.items.map(ri => ri.alias.get.name).toList
    case (None, None)                => defaultColumnNames
  }

  def defaultColumnNames: List[String] = defaultColumnSet.map(_.name)

  def yieldOrWhere: YieldOrWhere = None
  def yields: Option[Yield] = yieldOrWhere.flatMap(yw => yw.left.toOption.map { case (y, _) => y })
  def returns: Option[Return] = yieldOrWhere.flatMap(yw => yw.left.toOption.flatMap { case (_, r) => r })
  def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ReadAdministrationCommand

  override def returnColumns: List[LogicalVariable] = returnColumnNames.map(name => Variable(name)(position))

  override def semanticCheck: SemanticCheck = SemanticCheck.nestedCheck {

    def checkForSubquery(astNode: ASTNode): SemanticCheck = (state: SemanticState) => {
      val invalid: Option[Expression] = astNode.folder.treeFind[Expression] {
        case _: SubqueryExpression => true
      }
      invalid.map {
        case exp: ExistsExpression =>
          SemanticCheckResult.error(state, "The EXISTS expression is not valid on SHOW commands.", exp.position)
        case exp: CountExpression =>
          SemanticCheckResult.error(state, "The COUNT expression is not valid on SHOW commands.", exp.position)
        case exp: PatternExpression =>
          SemanticCheckResult.error(state, "Pattern expressions are not valid on SHOW commands.", exp.position)
        case exp: PatternComprehension =>
          SemanticCheckResult.error(state, "Pattern comprehensions are not valid on SHOW commands.", exp.position)
        case exp =>
          SemanticCheckResult.error(state, "Subquery expressions are not valid on SHOW commands.", exp.position)
      }.getOrElse(SemanticCheckResult.success(state))
    }

    def checkProjection(r: ProjectionClause): SemanticCheck = {
      val check = r.semanticCheck
      for {
        closingResult <- check
        continuationResult <- r.semanticCheckContinuation(closingResult.state.currentScope.scope)
      } yield {
        semantics.SemanticCheckResult(continuationResult.state, closingResult.errors ++ continuationResult.errors)
      }
    }

    val initialChecks: SemanticCheck = super.semanticCheck
      .chain((state: SemanticState) => SemanticCheckResult.success(state.newChildScope))
      .chain(
        // Create variables for the columns generated by the command
        semanticCheckFold(defaultColumnSet)(sc => declareVariable(sc.variable, sc.cypherType))
      )
      .chain(checkForSubquery(this))

    val projectionChecks = Seq(yields, returns).foldSemanticCheck {
      maybeClause =>
        maybeClause.foldSemanticCheck(r =>
          checkProjection(r).chain(recordCurrentScope(r))
        )
    }

    initialChecks chain projectionChecks
  }
}

sealed trait WriteAdministrationCommand extends AdministrationCommand {
  val isReadOnly: Boolean = false
  override def returnColumns: List[LogicalVariable] = List.empty
}

trait EitherAsString {

  def eitherAsString(either: Either[String, Parameter]): String = either match {
    case Left(u)  => u
    case Right(p) => s"$$${p.name}"
  }
}

// User commands

final case class ShowUsers(override val yieldOrWhere: YieldOrWhere, override val defaultColumnSet: List[ShowColumn])(
  val position: InputPosition
) extends ReadAdministrationCommand {

  override def name: String = "SHOW USERS"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowUsers =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowUsers {

  def apply(yieldOrWhere: YieldOrWhere)(position: InputPosition): ShowUsers =
    ShowUsers(
      yieldOrWhere,
      List(
        ShowColumn("user")(position),
        ShowColumn("roles", CTList(CTString))(position),
        ShowColumn("passwordChangeRequired", CTBoolean)(position),
        ShowColumn("suspended", CTBoolean)(position),
        ShowColumn("home")(position)
      )
    )(position)
}

final case class ShowCurrentUser(
  override val yieldOrWhere: YieldOrWhere,
  override val defaultColumnSet: List[ShowColumn]
)(val position: InputPosition) extends ReadAdministrationCommand {

  override def name: String = "SHOW CURRENT USER"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowCurrentUser =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowCurrentUser {

  def apply(yieldOrWhere: YieldOrWhere)(position: InputPosition): ShowCurrentUser =
    ShowCurrentUser(
      yieldOrWhere,
      List(
        ShowColumn("user")(position),
        ShowColumn("roles", CTList(CTString))(position),
        ShowColumn("passwordChangeRequired", CTBoolean)(position),
        ShowColumn("suspended", CTBoolean)(position),
        ShowColumn("home")(position)
      )
    )(position)
}

final case class CreateUser(
  userName: Either[String, Parameter],
  isEncryptedPassword: Boolean,
  initialPassword: Expression,
  userOptions: UserOptions,
  ifExistsDo: IfExistsDo
)(val position: InputPosition) extends WriteAdministrationCommand with EitherAsString {

  override def name: String = ifExistsDo match {
    case IfExistsReplace | IfExistsInvalidSyntax => "CREATE OR REPLACE USER"
    case _                                       => "CREATE USER"
  }

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax => error(
        s"Failed to create the specified user '$userAsString': cannot have both `OR REPLACE` and `IF NOT EXISTS`.",
        position
      )
    case _ =>
      super.semanticCheck chain
        SemanticState.recordCurrentScope(this)
  }

  private val userAsString: String = eitherAsString(userName)
}

final case class DropUser(userName: Either[String, Parameter], ifExists: Boolean)(val position: InputPosition)
    extends WriteAdministrationCommand {

  override def name = "DROP USER"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class RenameUser(
  fromUserName: Either[String, Parameter],
  toUserName: Either[String, Parameter],
  ifExists: Boolean
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name: String = "RENAME USER"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class AlterUser(
  userName: Either[String, Parameter],
  isEncryptedPassword: Option[Boolean],
  initialPassword: Option[Expression],
  userOptions: UserOptions,
  ifExists: Boolean
)(val position: InputPosition) extends WriteAdministrationCommand {

  assert(
    initialPassword.isDefined || userOptions.requirePasswordChange.isDefined || userOptions.suspended.isDefined || userOptions.homeDatabase.isDefined
  )

  if (userOptions.homeDatabase.isDefined && userOptions.homeDatabase.get == null) {
    assert(initialPassword.isEmpty && userOptions.requirePasswordChange.isEmpty && userOptions.suspended.isEmpty)
  }

  override def name = "ALTER USER"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class SetOwnPassword(newPassword: Expression, currentPassword: Expression)(val position: InputPosition)
    extends WriteAdministrationCommand {

  override def name = "ALTER CURRENT USER SET PASSWORD"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

sealed trait HomeDatabaseAction
case object RemoveHomeDatabaseAction extends HomeDatabaseAction
final case class SetHomeDatabaseAction(name: DatabaseName) extends HomeDatabaseAction

final case class UserOptions(
  requirePasswordChange: Option[Boolean],
  suspended: Option[Boolean],
  homeDatabase: Option[HomeDatabaseAction]
)

// Role commands

final case class ShowRoles(
  withUsers: Boolean,
  showAll: Boolean,
  override val yieldOrWhere: YieldOrWhere,
  override val defaultColumnSet: List[ShowColumn]
)(val position: InputPosition) extends ReadAdministrationCommand {

  override def name: String = if (showAll) "SHOW ALL ROLES" else "SHOW POPULATED ROLES"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowRoles =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowRoles {

  def apply(withUsers: Boolean, showAll: Boolean, yieldOrWhere: YieldOrWhere)(position: InputPosition): ShowRoles = {
    val defaultColumnSet =
      if (withUsers) List(
        ShowColumn(Variable("role")(position), CTString, "role"),
        ShowColumn(Variable("member")(position), CTString, "member")
      )
      else List(ShowColumn(Variable("role")(position), CTString, "role"))
    ShowRoles(withUsers, showAll, yieldOrWhere, defaultColumnSet)(position)
  }
}

final case class CreateRole(
  roleName: Either[String, Parameter],
  from: Option[Either[String, Parameter]],
  ifExistsDo: IfExistsDo
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name: String = ifExistsDo match {
    case IfExistsReplace | IfExistsInvalidSyntax => "CREATE OR REPLACE ROLE"
    case _                                       => "CREATE ROLE"
  }

  override def semanticCheck: SemanticCheck =
    ifExistsDo match {
      case IfExistsInvalidSyntax =>
        val name = Prettifier.escapeName(roleName)
        error(
          s"Failed to create the specified role '$name': cannot have both `OR REPLACE` and `IF NOT EXISTS`.",
          position
        )
      case _ =>
        super.semanticCheck chain
          SemanticState.recordCurrentScope(this)
    }
}

final case class DropRole(roleName: Either[String, Parameter], ifExists: Boolean)(val position: InputPosition)
    extends WriteAdministrationCommand {

  override def name = "DROP ROLE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class RenameRole(
  fromRoleName: Either[String, Parameter],
  toRoleName: Either[String, Parameter],
  ifExists: Boolean
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name: String = "RENAME ROLE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class GrantRolesToUsers(
  roleNames: Seq[Either[String, Parameter]],
  userNames: Seq[Either[String, Parameter]]
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name = "GRANT ROLE"

  override def semanticCheck: SemanticCheck = {
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
  }
}

final case class RevokeRolesFromUsers(
  roleNames: Seq[Either[String, Parameter]],
  userNames: Seq[Either[String, Parameter]]
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name = "REVOKE ROLE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

// Privilege commands

final case class ShowPrivileges(
  scope: ShowPrivilegeScope,
  override val yieldOrWhere: YieldOrWhere,
  override val defaultColumnSet: List[ShowColumn]
)(val position: InputPosition) extends ReadAdministrationCommand {
  override def name = "SHOW PRIVILEGE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowPrivileges =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowPrivileges {

  def apply(scope: ShowPrivilegeScope, yieldOrWhere: YieldOrWhere)(position: InputPosition): ShowPrivileges = {
    val columns = List(
      ShowColumn("access")(position),
      ShowColumn("action")(position),
      ShowColumn("resource")(position),
      ShowColumn("graph")(position),
      ShowColumn("segment")(position),
      ShowColumn("role")(position),
      ShowColumn("immutable", CTBoolean)(position)
    ) ++ (scope match {
      case _: ShowUserPrivileges | _: ShowUsersPrivileges => List(ShowColumn("user")(position))
      case _                                              => List.empty
    })
    ShowPrivileges(scope, yieldOrWhere, columns)(position)
  }
}

final case class ShowPrivilegeCommands(
  scope: ShowPrivilegeScope,
  asRevoke: Boolean,
  override val yieldOrWhere: YieldOrWhere,
  override val defaultColumnSet: List[ShowColumn]
)(val position: InputPosition) extends ReadAdministrationCommand {
  override def name = "SHOW PRIVILEGE COMMANDS"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowPrivilegeCommands =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowPrivilegeCommands {

  def apply(
    scope: ShowPrivilegeScope,
    asRevoke: Boolean,
    yieldOrWhere: YieldOrWhere
  )(position: InputPosition): ShowPrivilegeCommands = {
    val allColumns =
      List((ShowColumn("command")(position), true), (ShowColumn("immutable", CTBoolean)(position), false))
    val columns = DefaultOrAllShowColumns(allColumns, yieldOrWhere).columns
    ShowPrivilegeCommands(scope, asRevoke, yieldOrWhere, columns)(position)
  }
}

//noinspection ScalaUnusedSymbol
sealed abstract class PrivilegeCommand(
  privilege: PrivilegeType,
  qualifier: List[PrivilegeQualifier],
  position: InputPosition
) extends WriteAdministrationCommand {

  protected def immutableKeywordOrEmptyString(immutable: Boolean): String = if (immutable) " IMMUTABLE" else ""

  override def semanticCheck: SemanticCheck =
    privilege match {
      case DbmsPrivilege(u: UnassignableAction) =>
        error(s"`GRANT`, `DENY` and `REVOKE` are not supported for `${u.name}`", position)
      case GraphPrivilege(_, scope) if scope.exists {
          case _: DefaultGraphScope => true
          case _                    => false
        } => error("`ON DEFAULT GRAPH` is not supported. Use `ON HOME GRAPH` instead.", position)
      case DatabasePrivilege(_, scope) if scope.exists {
          case _: DefaultDatabaseScope => true
          case _                       => false
        } => error("`ON DEFAULT DATABASE` is not supported. Use `ON HOME DATABASE` instead.", position)
      case _ => super.semanticCheck chain
          SemanticState.recordCurrentScope(this)
    }
}

final case class GrantPrivilege(
  privilege: PrivilegeType,
  immutable: Boolean,
  resource: Option[ActionResource],
  qualifier: List[PrivilegeQualifier],
  roleNames: Seq[Either[String, Parameter]]
)(val position: InputPosition) extends PrivilegeCommand(privilege, qualifier, position) {
  override def name = s"GRANT${immutableKeywordOrEmptyString(immutable)} ${privilege.name}"
}

object GrantPrivilege {

  def dbmsAction(
    action: DbmsAction,
    immutable: Boolean,
    roleNames: Seq[Either[String, Parameter]],
    qualifier: List[PrivilegeQualifier] = List(AllQualifier()(InputPosition.NONE))
  ): InputPosition => GrantPrivilege =
    GrantPrivilege(DbmsPrivilege(action)(InputPosition.NONE), immutable, None, qualifier, roleNames)

  def databaseAction(
    action: DatabaseAction,
    immutable: Boolean,
    scope: List[DatabaseScope],
    roleNames: Seq[Either[String, Parameter]],
    qualifier: List[DatabasePrivilegeQualifier] = List(AllDatabasesQualifier()(InputPosition.NONE))
  ): InputPosition => GrantPrivilege =
    GrantPrivilege(DatabasePrivilege(action, scope)(InputPosition.NONE), immutable, None, qualifier, roleNames)

  def graphAction[T <: GraphPrivilegeQualifier](
    action: GraphAction,
    immutable: Boolean,
    resource: Option[ActionResource],
    scope: List[GraphScope],
    qualifier: List[T],
    roleNames: Seq[Either[String, Parameter]]
  ): InputPosition => GrantPrivilege =
    GrantPrivilege(GraphPrivilege(action, scope)(InputPosition.NONE), immutable, resource, qualifier, roleNames)
}

final case class DenyPrivilege(
  privilege: PrivilegeType,
  immutable: Boolean,
  resource: Option[ActionResource],
  qualifier: List[PrivilegeQualifier],
  roleNames: Seq[Either[String, Parameter]]
)(val position: InputPosition) extends PrivilegeCommand(privilege, qualifier, position) {

  override def name = s"DENY${immutableKeywordOrEmptyString(immutable)} ${privilege.name}"

  override def semanticCheck: SemanticCheck = {
    privilege match {
      case GraphPrivilege(MergeAdminAction, _) =>
        error(s"`DENY MERGE` is not supported. Use `DENY SET PROPERTY` and `DENY CREATE` instead.", position)
      case _ => super.semanticCheck
    }
  }
}

object DenyPrivilege {

  def dbmsAction(
    action: DbmsAction,
    immutable: Boolean,
    roleNames: Seq[Either[String, Parameter]],
    qualifier: List[PrivilegeQualifier] = List(AllQualifier()(InputPosition.NONE))
  ): InputPosition => DenyPrivilege =
    DenyPrivilege(DbmsPrivilege(action)(InputPosition.NONE), immutable, None, qualifier, roleNames)

  def databaseAction(
    action: DatabaseAction,
    immutable: Boolean,
    scope: List[DatabaseScope],
    roleNames: Seq[Either[String, Parameter]],
    qualifier: List[DatabasePrivilegeQualifier] = List(AllDatabasesQualifier()(InputPosition.NONE))
  ): InputPosition => DenyPrivilege =
    DenyPrivilege(DatabasePrivilege(action, scope)(InputPosition.NONE), immutable, None, qualifier, roleNames)

  def graphAction[T <: GraphPrivilegeQualifier](
    action: GraphAction,
    immutable: Boolean,
    resource: Option[ActionResource],
    scope: List[GraphScope],
    qualifier: List[T],
    roleNames: Seq[Either[String, Parameter]]
  ): InputPosition => DenyPrivilege =
    DenyPrivilege(GraphPrivilege(action, scope)(InputPosition.NONE), immutable, resource, qualifier, roleNames)
}

final case class RevokePrivilege(
  privilege: PrivilegeType,
  immutableOnly: Boolean,
  resource: Option[ActionResource],
  qualifier: List[PrivilegeQualifier],
  roleNames: Seq[Either[String, Parameter]],
  revokeType: RevokeType
)(val position: InputPosition) extends PrivilegeCommand(privilege, qualifier, position) {

  override def name: String = {
    val revokeTypeOrEmptyString = if (revokeType.name.nonEmpty) s" ${revokeType.name}" else ""
    s"REVOKE$revokeTypeOrEmptyString${immutableKeywordOrEmptyString(immutableOnly)} ${privilege.name}"
  }

  override def semanticCheck: SemanticCheck = {
    (privilege, revokeType) match {
      case (GraphPrivilege(MergeAdminAction, _), RevokeDenyType()) =>
        error(s"`DENY MERGE` is not supported. Use `DENY SET PROPERTY` and `DENY CREATE` instead.", position)
      case _ => super.semanticCheck
    }
  }

}

object RevokePrivilege {

  def dbmsAction(
    action: DbmsAction,
    immutable: Boolean,
    roleNames: Seq[Either[String, Parameter]],
    revokeType: RevokeType,
    qualifier: List[PrivilegeQualifier] = List(AllQualifier()(InputPosition.NONE))
  ): InputPosition => RevokePrivilege =
    RevokePrivilege(DbmsPrivilege(action)(InputPosition.NONE), immutable, None, qualifier, roleNames, revokeType)

  def databaseAction(
    action: DatabaseAction,
    immutable: Boolean,
    scope: List[DatabaseScope],
    roleNames: Seq[Either[String, Parameter]],
    revokeType: RevokeType,
    qualifier: List[DatabasePrivilegeQualifier] = List(AllDatabasesQualifier()(InputPosition.NONE))
  ): InputPosition => RevokePrivilege =
    RevokePrivilege(
      DatabasePrivilege(action, scope)(InputPosition.NONE),
      immutable,
      None,
      qualifier,
      roleNames,
      revokeType
    )

  def graphAction[T <: GraphPrivilegeQualifier](
    action: GraphAction,
    immutable: Boolean,
    resource: Option[ActionResource],
    scope: List[GraphScope],
    qualifier: List[T],
    roleNames: Seq[Either[String, Parameter]],
    revokeType: RevokeType
  ): InputPosition => RevokePrivilege =
    RevokePrivilege(
      GraphPrivilege(action, scope)(InputPosition.NONE),
      immutable,
      resource,
      qualifier,
      roleNames,
      revokeType
    )
}

// Server commands

final case class EnableServer(serverName: Either[String, Parameter], optionsMap: Options)(
  val position: InputPosition
) extends WriteAdministrationCommand {
  override def name: String = "ENABLE SERVER"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class DropServer(serverName: Either[String, Parameter])(
  val position: InputPosition
) extends WriteAdministrationCommand {
  override def name: String = "DROP SERVER"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class ShowServers(override val yieldOrWhere: YieldOrWhere, defaultColumns: DefaultOrAllShowColumns)(
  val position: InputPosition
) extends ReadAdministrationCommand {
  override val defaultColumnSet: List[ShowColumn] = defaultColumns.columns

  override def name: String = "SHOW SERVERS"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowServers =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowServers {

  def apply(yieldOrWhere: YieldOrWhere)(position: InputPosition): ShowServers = {
    val showColumns = List(
      (ShowColumn("serverId")(position), false),
      (ShowColumn("name")(position), true),
      (ShowColumn("address")(position), true),
      (ShowColumn("httpAddress")(position), false),
      (ShowColumn("httpsAddress")(position), false),
      (ShowColumn("state")(position), true),
      (ShowColumn("health")(position), true),
      (ShowColumn("hosting", CTList(CTString))(position), true),
      (ShowColumn("requestedHosting", CTList(CTString))(position), false),
      (ShowColumn("tags", CTList(CTString))(position), false),
      (ShowColumn("allowedDatabases", CTList(CTString))(position), false),
      (ShowColumn("deniedDatabases", CTList(CTString))(position), false),
      (ShowColumn("modeConstraint")(position), false),
      (ShowColumn("version")(position), false)
    )
    val briefShowColumns = showColumns.filter(_._2).map(_._1)
    val allShowColumns = showColumns.map(_._1)

    val allColumns = yieldOrWhere match {
      case Some(Left(_)) => true
      case _             => false
    }
    val columns = DefaultOrAllShowColumns(allColumns, briefShowColumns, allShowColumns)
    ShowServers(yieldOrWhere, columns)(position)
  }
}

final case class DeallocateServers(serverNames: Seq[Either[String, Parameter]])(
  val position: InputPosition
) extends WriteAdministrationCommand {
  override def name: String = "DEALLOCATE DATABASES FROM SERVER"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

// Database commands

final case class ShowDatabase(
  scope: DatabaseScope,
  override val yieldOrWhere: YieldOrWhere,
  defaultColumns: DefaultOrAllShowColumns
)(val position: InputPosition) extends ReadAdministrationCommand {
  override val defaultColumnSet: List[ShowColumn] = defaultColumns.columns

  override def name: String = scope match {
    case _: NamedDatabaseScope   => "SHOW DATABASE"
    case _: AllDatabasesScope    => "SHOW DATABASES"
    case _: DefaultDatabaseScope => "SHOW DEFAULT DATABASE"
    case _: HomeDatabaseScope    => "SHOW HOME DATABASE"
  }

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowDatabase =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowDatabase {

  val NAME_COL = "name"
  val ALIASES_COL = "aliases"
  val TYPE_COL = "type"
  val ACCESS_COL = "access"
  val DATABASE_ID_COL = "databaseID"
  val SERVER_ID_COL = "serverID"
  val ADDRESS_COL = "address"
  val ROLE_COL = "role"
  val WRITER_COL = "writer"
  val CURRENT_STATUS_COL = "currentStatus"
  val REQUESTED_STATUS_COL = "requestedStatus"
  val STATUS_MSG_COL = "statusMessage"
  val DEFAULT_COL = "default"
  val HOME_COL = "home"
  val CURRENT_PRIMARIES_COUNT_COL = "currentPrimariesCount"
  val CURRENT_SECONDARIES_COUNT_COL = "currentSecondariesCount"
  val REQUESTED_PRIMARIES_COUNT_COL = "requestedPrimariesCount"
  val REQUESTED_SECONDARIES_COUNT_COL = "requestedSecondariesCount"
  val CREATION_TIME_COL = "creationTime"
  val LAST_START_TIME_COL = "lastStartTime"
  val LAST_STOP_TIME_COL = "lastStopTime"
  val STORE_COL = "store"
  val LAST_COMMITTED_TX_COL = "lastCommittedTxn"
  val REPLICATION_LAG_COL = "replicationLag"
  val CONSTITUENTS_COL = "constituents"

  def apply(scope: DatabaseScope, yieldOrWhere: YieldOrWhere)(position: InputPosition): ShowDatabase = {
    val showColumns = List(
      // (column, brief)
      (ShowColumn(NAME_COL)(position), true),
      (ShowColumn(TYPE_COL)(position), true),
      (ShowColumn(ALIASES_COL, CTList(CTString))(position), true),
      (ShowColumn(ACCESS_COL)(position), true),
      (ShowColumn(DATABASE_ID_COL)(position), false),
      (ShowColumn(SERVER_ID_COL)(position), false),
      (ShowColumn(ADDRESS_COL)(position), true),
      (ShowColumn(ROLE_COL)(position), true),
      (ShowColumn(WRITER_COL, CTBoolean)(position), true),
      (ShowColumn(REQUESTED_STATUS_COL)(position), true),
      (ShowColumn(CURRENT_STATUS_COL)(position), true),
      (ShowColumn(STATUS_MSG_COL)(position), true)
    ) ++ (scope match {
      case _: DefaultDatabaseScope => List.empty
      case _: HomeDatabaseScope    => List.empty
      case _ =>
        List((ShowColumn(DEFAULT_COL, CTBoolean)(position), true), (ShowColumn(HOME_COL, CTBoolean)(position), true))
    }) ++ List(
      (ShowColumn(CURRENT_PRIMARIES_COUNT_COL, CTInteger)(position), false),
      (ShowColumn(CURRENT_SECONDARIES_COUNT_COL, CTInteger)(position), false),
      (ShowColumn(REQUESTED_PRIMARIES_COUNT_COL, CTInteger)(position), false),
      (ShowColumn(REQUESTED_SECONDARIES_COUNT_COL, CTInteger)(position), false),
      (ShowColumn(CREATION_TIME_COL, CTDateTime)(position), false),
      (ShowColumn(LAST_START_TIME_COL, CTDateTime)(position), false),
      (ShowColumn(LAST_STOP_TIME_COL, CTDateTime)(position), false),
      (ShowColumn(STORE_COL)(position), false),
      (ShowColumn(LAST_COMMITTED_TX_COL, CTInteger)(position), false),
      (ShowColumn(REPLICATION_LAG_COL, CTInteger)(position), false),
      (ShowColumn(CONSTITUENTS_COL, CTList(CTString))(position), true)
    )

    ShowDatabase(scope, yieldOrWhere, DefaultOrAllShowColumns(showColumns, yieldOrWhere))(position)
  }
}

final case class CreateDatabase(
  dbName: DatabaseName,
  ifExistsDo: IfExistsDo,
  options: Options,
  waitUntilComplete: WaitUntilComplete
)(val position: InputPosition)
    extends WaitableAdministrationCommand {

  override def name: String = ifExistsDo match {
    case IfExistsReplace | IfExistsInvalidSyntax => "CREATE OR REPLACE DATABASE"
    case _                                       => "CREATE DATABASE"
  }

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax =>
      val name = Prettifier.escapeName(dbName)
      error(
        s"Failed to create the specified database '$name': cannot have both `OR REPLACE` and `IF NOT EXISTS`.",
        position
      )
    case _ =>
      super.semanticCheck chain
        SemanticState.recordCurrentScope(this)
  }
}

final case class CreateCompositeDatabase(
  databaseName: DatabaseName,
  ifExistsDo: IfExistsDo,
  waitUntilComplete: WaitUntilComplete
)(
  val position: InputPosition
) extends WriteAdministrationCommand {

  override def name: String = ifExistsDo match {
    case IfExistsReplace | IfExistsInvalidSyntax => "CREATE OR REPLACE COMPOSITE DATABASE"
    case _                                       => "CREATE COMPOSITE DATABASE"
  }

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax =>
      val name = Prettifier.escapeName(databaseName)
      error(
        s"Failed to create the specified composite database '$name': cannot have both `OR REPLACE` and `IF NOT EXISTS`.",
        position
      )
    case _ =>
      databaseName match {
        case nsn @ NamespacedName(_, Some(_)) =>
          error(
            s"Failed to create the specified composite database '${nsn.toString}': COMPOSITE DATABASE names cannot contain \".\". " +
              "COMPOSITE DATABASE names using '.' must be quoted with backticks e.g. `composite.database`.",
            nsn.position
          )
        case _ => super.semanticCheck
      }
  }
}

final case class DropDatabase(
  dbName: DatabaseName,
  ifExists: Boolean,
  composite: Boolean,
  additionalAction: DropDatabaseAdditionalAction,
  waitUntilComplete: WaitUntilComplete
)(val position: InputPosition) extends WaitableAdministrationCommand {

  override def name: String = if (composite) "DROP COMPOSITE DATABASE" else "DROP DATABASE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class AlterDatabase(dbName: DatabaseName, ifExists: Boolean, access: Access)(
  val position: InputPosition
) extends WriteAdministrationCommand {

  override def name = "ALTER DATABASE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class StartDatabase(dbName: DatabaseName, waitUntilComplete: WaitUntilComplete)(
  val position: InputPosition
) extends WaitableAdministrationCommand {

  override def name = "START DATABASE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

final case class StopDatabase(dbName: DatabaseName, waitUntilComplete: WaitUntilComplete)(
  val position: InputPosition
) extends WaitableAdministrationCommand {

  override def name = "STOP DATABASE"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}

sealed trait WaitableAdministrationCommand extends WriteAdministrationCommand {
  val waitUntilComplete: WaitUntilComplete

  override def returnColumns: List[LogicalVariable] = waitUntilComplete match {
    case NoWait => List.empty
    case _      => List("address", "state", "message", "success").map(Variable(_)(position))
  }
}

sealed trait WaitUntilComplete {
  val DEFAULT_TIMEOUT = 300L
  val name: String
  def timeout: Long = DEFAULT_TIMEOUT
}

case object NoWait extends WaitUntilComplete {
  override val name: String = ""
}

case object IndefiniteWait extends WaitUntilComplete {
  override val name: String = " WAIT"
}

case class TimeoutAfter(timoutSeconds: Long) extends WaitUntilComplete {
  override val name: String = s" WAIT $timoutSeconds SECONDS"
  override def timeout: Long = timoutSeconds
}

sealed trait Access
case object ReadOnlyAccess extends Access
case object ReadWriteAccess extends Access

sealed abstract class DropDatabaseAdditionalAction(val name: String)
case object DumpData extends DropDatabaseAdditionalAction("DUMP DATA")
case object DestroyData extends DropDatabaseAdditionalAction("DESTROY DATA")

final case class ShowAliases(
  aliasName: Option[DatabaseName],
  override val yieldOrWhere: YieldOrWhere,
  defaultColumns: DefaultOrAllShowColumns
)(
  val position: InputPosition
) extends ReadAdministrationCommand {
  override val defaultColumnSet: List[ShowColumn] = defaultColumns.columns

  override def name: String = aliasName match {
    case None    => "SHOW ALIASES"
    case Some(_) => "SHOW ALIAS"
  }

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)

  override def withYieldOrWhere(newYieldOrWhere: YieldOrWhere): ShowAliases =
    this.copy(yieldOrWhere = newYieldOrWhere)(position)
}

object ShowAliases {

  def apply(yieldOrWhere: YieldOrWhere)(position: InputPosition): ShowAliases = apply(None, yieldOrWhere)(position)

  def apply(
    aliasName: Option[DatabaseName],
    yieldOrWhere: YieldOrWhere
  )(position: InputPosition): ShowAliases = {
    val showColumns = List(
      // (column, brief)
      (ShowColumn("name")(position), true),
      (ShowColumn("database")(position), true),
      (ShowColumn("location")(position), true),
      (ShowColumn("url")(position), true),
      (ShowColumn("user")(position), true),
      (ShowColumn("driver", CTMap)(position), false),
      (ShowColumn("properties", CTMap)(position), false)
    )

    ShowAliases(aliasName, yieldOrWhere, DefaultOrAllShowColumns(showColumns, yieldOrWhere))(position)
  }
}

object AliasDriverSettingsCheck {
  val existsErrorMessage = "The EXISTS expression is not valid in driver settings."
  val countErrorMessage = "The COUNT expression is not valid in driver settings."

  def findInvalidDriverSettings(driverSettings: Option[Either[Map[String, Expression], Parameter]])
    : Option[Expression] = {
    driverSettings match {
      case Some(Left(settings)) =>
        settings.values.flatMap(s =>
          s.folder.treeFind[Expression] {
            case _: ExistsExpression => true
            case _: CountExpression  => true
          }
        ).headOption
      case _ => None
    }
  }
}

final case class CreateLocalDatabaseAlias(
  aliasName: DatabaseName,
  targetName: DatabaseName,
  ifExistsDo: IfExistsDo,
  properties: Option[Either[Map[String, Expression], Parameter]] = None
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name: String = ifExistsDo match {
    case IfExistsReplace | IfExistsInvalidSyntax => "CREATE OR REPLACE ALIAS"
    case _                                       => "CREATE ALIAS"
  }

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax => error(
        s"Failed to create the specified alias '${Prettifier.escapeName(aliasName)}': cannot have both `OR REPLACE` and `IF NOT EXISTS`.",
        position
      )
    case _ => super.semanticCheck chain
        namespacedNameHasNoDots chain
        SemanticState.recordCurrentScope(this)
  }

  private def namespacedNameHasNoDots: SemanticCheck = aliasName match {
    case nsn @ NamespacedName(nameComponents, Some(_)) =>
      if (nameComponents.length > 1) error(
        s"'.' is not a valid character in the local alias name '${nsn.toString}'. " +
          "Local alias names using '.' must be quoted with backticks when adding a local alias to a composite database e.g. `local.alias`.",
        nsn.position
      )
      else success
    case _ => success
  }
}

final case class CreateRemoteDatabaseAlias(
  aliasName: DatabaseName,
  targetName: DatabaseName,
  ifExistsDo: IfExistsDo,
  url: Either[String, Parameter],
  username: Either[String, Parameter],
  password: Expression,
  driverSettings: Option[Either[Map[String, Expression], Parameter]] = None,
  properties: Option[Either[Map[String, Expression], Parameter]] = None
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name: String = ifExistsDo match {
    case IfExistsReplace | IfExistsInvalidSyntax => "CREATE OR REPLACE ALIAS"
    case _                                       => "CREATE ALIAS"
  }

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax => error(
        s"Failed to create the specified alias '${Prettifier.escapeName(aliasName)}': cannot have both `OR REPLACE` and `IF NOT EXISTS`.",
        position
      )
    case _ => AliasDriverSettingsCheck.findInvalidDriverSettings(driverSettings) match {
        case Some(expr: ExistsExpression) =>
          error(AliasDriverSettingsCheck.existsErrorMessage, expr.position)
        case Some(expr) =>
          error(AliasDriverSettingsCheck.countErrorMessage, expr.position)
        case _ => super.semanticCheck chain SemanticState.recordCurrentScope(this)
      }
  }
}

final case class AlterLocalDatabaseAlias(
  aliasName: DatabaseName,
  targetName: Option[DatabaseName],
  ifExists: Boolean = false,
  properties: Option[Either[Map[String, Expression], Parameter]] = None
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name = "ALTER ALIAS"

  override def semanticCheck: SemanticCheck = super.semanticCheck chain SemanticState.recordCurrentScope(this)
}

final case class AlterRemoteDatabaseAlias(
  aliasName: DatabaseName,
  targetName: Option[DatabaseName] = None,
  ifExists: Boolean = false,
  url: Option[Either[String, Parameter]] = None,
  username: Option[Either[String, Parameter]] = None,
  password: Option[Expression] = None,
  driverSettings: Option[Either[Map[String, Expression], Parameter]] = None,
  properties: Option[Either[Map[String, Expression], Parameter]] = None
)(val position: InputPosition) extends WriteAdministrationCommand {

  override def name = "ALTER ALIAS"

  override def semanticCheck: SemanticCheck =
    AliasDriverSettingsCheck.findInvalidDriverSettings(driverSettings) match {
      case Some(expr) =>
        expr match {
          case _: ExistsExpression =>
            error(AliasDriverSettingsCheck.existsErrorMessage, expr.position)
          case _ =>
            error(AliasDriverSettingsCheck.countErrorMessage, expr.position)
        }
      case _ =>
        val isLocalAlias = targetName.isDefined && url.isEmpty
        val isRemoteAlias = url.isDefined || username.isDefined || password.isDefined || driverSettings.isDefined
        if (isLocalAlias && isRemoteAlias) {
          error(
            s"Failed to alter the specified database alias '${Prettifier.escapeName(aliasName)}': url needs to be defined to alter a remote alias target.",
            position
          )
        } else {
          super.semanticCheck chain SemanticState.recordCurrentScope(this)
        }
    }
}

final case class DropDatabaseAlias(aliasName: DatabaseName, ifExists: Boolean)(
  val position: InputPosition
) extends WriteAdministrationCommand {

  override def name = "DROP ALIAS"

  override def semanticCheck: SemanticCheck =
    super.semanticCheck chain
      SemanticState.recordCurrentScope(this)
}
