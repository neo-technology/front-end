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

import org.opencypher.v9_0.ast.semantics.SemanticAnalysisTooling
import org.opencypher.v9_0.ast.semantics.SemanticCheck
import org.opencypher.v9_0.ast.semantics.SemanticCheck.when
import org.opencypher.v9_0.ast.semantics.SemanticExpressionCheck
import org.opencypher.v9_0.expressions.FunctionInvocation
import org.opencypher.v9_0.expressions.FunctionName
import org.opencypher.v9_0.expressions.LabelName
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.Property
import org.opencypher.v9_0.expressions.PropertyKeyName
import org.opencypher.v9_0.expressions.RelTypeName
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.expressions.functions.Labels
import org.opencypher.v9_0.expressions.functions.Type
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.CTNode
import org.opencypher.v9_0.util.symbols.CTRelationship
import org.opencypher.v9_0.util.symbols.CypherType
import org.opencypher.v9_0.util.symbols.NodeType
import org.opencypher.v9_0.util.symbols.RelationshipType

sealed trait SchemaCommand extends StatementWithGraph with SemanticAnalysisTooling {
  def useGraph: Option[GraphSelection]
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand

  override def returnColumns: List[LogicalVariable] = List.empty

  override def containsUpdates: Boolean = true

  // The validation of the values (provider, config keys and config values) are done at runtime.
  protected def checkOptionsMap(schemaString: String, options: Options): SemanticCheck = options match {
    case OptionsMap(ops)
      if ops.view.filterKeys(k =>
        !k.equalsIgnoreCase("indexProvider") && !k.equalsIgnoreCase("indexConfig")
      ).nonEmpty =>
      error(
        s"Failed to create $schemaString: Invalid option provided, valid options are `indexProvider` and `indexConfig`.",
        position
      )
    case _ => SemanticCheck.success
  }

  protected def checkSingleProperty(schemaString: String, properties: List[Property]): SemanticCheck =
    when(properties.size > 1) {
      error(s"Only single property $schemaString are supported", properties(1).position)
    }

  // Error messages for mixing old and new constraint syntax
  protected val errorMessageOnRequire: String =
    "Invalid constraint syntax, ON should not be used in combination with REQUIRE. Replace ON with FOR."

  protected val errorMessageForAssert: String =
    "Invalid constraint syntax, FOR should not be used in combination with ASSERT. Replace ASSERT with REQUIRE."

  protected val errorMessageForAssertExists: String =
    "Invalid constraint syntax, FOR should not be used in combination with ASSERT EXISTS. Replace ASSERT EXISTS with REQUIRE ... IS NOT NULL."

  protected val errorMessageOnAssert: String =
    "Invalid constraint syntax, ON and ASSERT should not be used. Replace ON with FOR and ASSERT with REQUIRE."

  protected val errorMessageOnAssertExists: String =
    "Invalid constraint syntax, ON and ASSERT EXISTS should not be used. Replace ON with FOR and ASSERT EXISTS with REQUIRE ... IS NOT NULL."
}

case class CreateIndexOldSyntax(label: LabelName, properties: List[PropertyKeyName], useGraph: Option[UseGraph] = None)(
  val position: InputPosition
) extends SchemaCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  def semanticCheck: SemanticCheck = error("Invalid create index syntax, use `CREATE INDEX FOR ...` instead.", position)
}

abstract class CreateIndex(
  variable: Variable,
  properties: List[Property],
  ifExistsDo: IfExistsDo,
  isNodeIndex: Boolean
)(val position: InputPosition)
    extends SchemaCommand {

  // To anonymize the name
  val name: Option[String]
  def withName(name: Option[String]): CreateIndex

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax | IfExistsReplace =>
      error("Failed to create index: `OR REPLACE` cannot be used together with this command.", position)
    case _ =>
      val ctType = if (isNodeIndex) CTNode else CTRelationship
      declareVariable(variable, ctType) chain
        SemanticExpressionCheck.simple(properties) chain
        semanticCheckFold(properties) {
          property =>
            when(!property.map.isInstanceOf[Variable]) {
              error("Cannot index nested properties", property.position)
            }
        }
  }
}

case class CreateBtreeNodeIndex(
  variable: Variable,
  label: LabelName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, true)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateBtreeNodeIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    error("Invalid index type b-tree, use range, point or text index instead.", position)
}

case class CreateBtreeRelationshipIndex(
  variable: Variable,
  relType: RelTypeName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, false)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateBtreeRelationshipIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    error("Invalid index type b-tree, use range, point or text index instead.", position)
}

case class CreateRangeNodeIndex(
  variable: Variable,
  label: LabelName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  fromDefault: Boolean,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, true)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateRangeNodeIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    checkOptionsMap("range node property index", options) chain super.semanticCheck
}

case class CreateRangeRelationshipIndex(
  variable: Variable,
  relType: RelTypeName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  fromDefault: Boolean,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, false)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateRangeRelationshipIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    checkOptionsMap("range relationship property index", options) chain super.semanticCheck
}

case class CreateLookupIndex(
  variable: Variable,
  isNodeIndex: Boolean,
  function: FunctionInvocation,
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, List.empty, ifExistsDo, isNodeIndex)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateLookupIndex = copy(name = name)(position)

  private def allowedFunction(name: String): Boolean =
    if (isNodeIndex) name.equalsIgnoreCase(Labels.name) else name.equalsIgnoreCase(Type.name)

  override def semanticCheck: SemanticCheck = function match {
    case FunctionInvocation(_, FunctionName(name), _, _) if !allowedFunction(name) =>
      if (isNodeIndex) error(
        s"Failed to create node lookup index: Function '$name' is not allowed, valid function is '${Labels.name}'.",
        position
      )
      else error(
        s"Failed to create relationship lookup index: Function '$name' is not allowed, valid function is '${Type.name}'.",
        position
      )
    case _ =>
      checkOptionsMap("token lookup index", options) chain super.semanticCheck chain SemanticExpressionCheck.simple(
        function
      )
  }
}

case class CreateFulltextNodeIndex(
  variable: Variable,
  label: List[LabelName],
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, true)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateFulltextNodeIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck = checkOptionsMap("fulltext node index", options) chain super.semanticCheck
}

case class CreateFulltextRelationshipIndex(
  variable: Variable,
  relType: List[RelTypeName],
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, false)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateFulltextRelationshipIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    checkOptionsMap("fulltext relationship index", options) chain super.semanticCheck
}

case class CreateTextNodeIndex(
  variable: Variable,
  label: LabelName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, true)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateTextNodeIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    checkOptionsMap("text node index", options) chain
      super.semanticCheck chain
      checkSingleProperty("text indexes", properties)
}

case class CreateTextRelationshipIndex(
  variable: Variable,
  relType: RelTypeName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, false)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateTextRelationshipIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    checkOptionsMap("text relationship index", options) chain
      super.semanticCheck chain
      checkSingleProperty("text indexes", properties)
}

case class CreatePointNodeIndex(
  variable: Variable,
  label: LabelName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, true)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreatePointNodeIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    checkOptionsMap("point node index", options) chain
      super.semanticCheck chain
      checkSingleProperty("point indexes", properties)
}

case class CreatePointRelationshipIndex(
  variable: Variable,
  relType: RelTypeName,
  properties: List[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  useGraph: Option[GraphSelection] = None
)(override val position: InputPosition)
    extends CreateIndex(variable, properties, ifExistsDo, false)(position) {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreatePointRelationshipIndex = copy(name = name)(position)

  override def semanticCheck: SemanticCheck =
    checkOptionsMap("point relationship index", options) chain
      super.semanticCheck chain
      checkSingleProperty("point indexes", properties)
}

case class DropIndex(label: LabelName, properties: List[PropertyKeyName], useGraph: Option[GraphSelection] = None)(
  val position: InputPosition
) extends SchemaCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)

  def semanticCheck: SemanticCheck = error(
    "Indexes cannot be dropped by schema, please drop by name instead: DROP INDEX index_name. The index name can be found using SHOW INDEXES.",
    position
  )
}

case class DropIndexOnName(name: String, ifExists: Boolean, useGraph: Option[GraphSelection] = None)(
  val position: InputPosition
) extends SchemaCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  def semanticCheck = Seq()
}

trait PropertyConstraintCommand extends SchemaCommand {
  def variable: Variable

  def property: Property

  def entityType: CypherType

  override def semanticCheck: SemanticCheck =
    declareVariable(variable, entityType) chain
      SemanticExpressionCheck.simple(property) chain
      when(!property.map.isInstanceOf[Variable]) {
        error("Cannot index nested properties", property.position)
      }
}

trait CompositePropertyConstraintCommand extends SchemaCommand {
  def variable: Variable

  def properties: Seq[Property]

  def entityType: CypherType

  override def semanticCheck: SemanticCheck =
    declareVariable(variable, entityType) chain
      SemanticExpressionCheck.simple(properties) chain
      semanticCheckFold(properties) {
        property =>
          when(!property.map.isInstanceOf[Variable]) {
            error("Cannot index nested properties", property.position)
          }
      }
}

trait NodePropertyConstraintCommand extends PropertyConstraintCommand {

  val entityType: NodeType = CTNode

  def label: LabelName
}

trait UniquePropertyConstraintCommand extends CompositePropertyConstraintCommand {

  val entityType: NodeType = CTNode

  def label: LabelName
}

trait NodeKeyConstraintCommand extends CompositePropertyConstraintCommand {

  val entityType: NodeType = CTNode

  def label: LabelName
}

trait RelationshipPropertyConstraintCommand extends PropertyConstraintCommand {

  val entityType: RelationshipType = CTRelationship

  def relType: RelTypeName
}

trait CreateConstraint extends SchemaCommand {
  // To anonymize the name
  val name: Option[String]
  def withName(name: Option[String]): CreateConstraint
}

case class CreateNodeKeyConstraint(
  variable: Variable,
  label: LabelName,
  properties: Seq[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  containsOn: Boolean,
  constraintVersion: ConstraintVersion,
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends NodeKeyConstraintCommand with CreateConstraint {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateNodeKeyConstraint = copy(name = name)(position)

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax | IfExistsReplace =>
      error(s"Failed to create node key constraint: `OR REPLACE` cannot be used together with this command.", position)
    case _ =>
      constraintVersion match {
        case ConstraintVersion2 if containsOn  => error(errorMessageOnRequire, position) // ON ... REQUIRE
        case ConstraintVersion0 if !containsOn => error(errorMessageForAssert, position) // FOR ... ASSERT
        case ConstraintVersion0 if containsOn  => error(errorMessageOnAssert, position) // ON ... ASSERT
        case _ => checkOptionsMap("node key constraint", options) chain super.semanticCheck
      }
  }
}

case class DropNodeKeyConstraint(
  variable: Variable,
  label: LabelName,
  properties: Seq[Property],
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends NodeKeyConstraintCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)

  override def semanticCheck: SemanticCheck = error(
    "Node key constraints cannot be dropped by schema, please drop by name instead: DROP CONSTRAINT constraint_name. The constraint name can be found using SHOW CONSTRAINTS.",
    position
  )
}

case class CreateUniquePropertyConstraint(
  variable: Variable,
  label: LabelName,
  properties: Seq[Property],
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  containsOn: Boolean,
  constraintVersion: ConstraintVersion,
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends UniquePropertyConstraintCommand with CreateConstraint {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateUniquePropertyConstraint = copy(name = name)(position)

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax | IfExistsReplace => error(
        s"Failed to create uniqueness constraint: `OR REPLACE` cannot be used together with this command.",
        position
      )
    case _ =>
      constraintVersion match {
        case ConstraintVersion2 if containsOn  => error(errorMessageOnRequire, position) // ON ... REQUIRE
        case ConstraintVersion0 if !containsOn => error(errorMessageForAssert, position) // FOR ... ASSERT
        case ConstraintVersion0 if containsOn  => error(errorMessageOnAssert, position) // ON ... ASSERT
        case _ => checkOptionsMap("uniqueness constraint", options) chain super.semanticCheck
      }
  }
}

case class DropUniquePropertyConstraint(
  variable: Variable,
  label: LabelName,
  properties: Seq[Property],
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends UniquePropertyConstraintCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)

  override def semanticCheck: SemanticCheck = error(
    "Uniqueness constraints cannot be dropped by schema, please drop by name instead: DROP CONSTRAINT constraint_name. The constraint name can be found using SHOW CONSTRAINTS.",
    position
  )
}

case class CreateNodePropertyExistenceConstraint(
  variable: Variable,
  label: LabelName,
  property: Property,
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  containsOn: Boolean,
  constraintVersion: ConstraintVersion,
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends NodePropertyConstraintCommand with CreateConstraint {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  override def withName(name: Option[String]): CreateNodePropertyExistenceConstraint = copy(name = name)(position)

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax | IfExistsReplace => error(
        s"Failed to create node property existence constraint: `OR REPLACE` cannot be used together with this command.",
        position
      )
    case _ =>
      constraintVersion match {
        case ConstraintVersion2 if containsOn =>
          error(errorMessageOnRequire, position) // ON ... REQUIRE ... IS NOT NULL
        case ConstraintVersion1 if !containsOn =>
          error(errorMessageForAssert, position) // FOR ... ASSERT ... IS NOT NULL
        case ConstraintVersion0 if !containsOn =>
          error(errorMessageForAssertExists, position) // FOR ... ASSERT EXISTS ...
        case ConstraintVersion1 if containsOn => error(errorMessageOnAssert, position) // ON ... ASSERT ... IS NOT NULL
        case ConstraintVersion0 if containsOn => error(errorMessageOnAssertExists, position) // ON ... ASSERT EXISTS ...
        case _ => checkOptionsMap("node property existence constraint", options) chain super.semanticCheck
      }
  }
}

case class DropNodePropertyExistenceConstraint(
  variable: Variable,
  label: LabelName,
  property: Property,
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends NodePropertyConstraintCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)

  override def semanticCheck: SemanticCheck = error(
    "Node property existence constraints cannot be dropped by schema, please drop by name instead: DROP CONSTRAINT constraint_name. The constraint name can be found using SHOW CONSTRAINTS.",
    position
  )
}

case class CreateRelationshipPropertyExistenceConstraint(
  variable: Variable,
  relType: RelTypeName,
  property: Property,
  override val name: Option[String],
  ifExistsDo: IfExistsDo,
  options: Options,
  containsOn: Boolean,
  constraintVersion: ConstraintVersion,
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends RelationshipPropertyConstraintCommand with CreateConstraint {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)

  override def withName(name: Option[String]): CreateRelationshipPropertyExistenceConstraint =
    copy(name = name)(position)

  override def semanticCheck: SemanticCheck = ifExistsDo match {
    case IfExistsInvalidSyntax | IfExistsReplace => error(
        s"Failed to create relationship property existence constraint: `OR REPLACE` cannot be used together with this command.",
        position
      )
    case _ =>
      constraintVersion match {
        case ConstraintVersion2 if containsOn =>
          error(errorMessageOnRequire, position) // ON ... REQUIRE ... IS NOT NULL
        case ConstraintVersion1 if !containsOn =>
          error(errorMessageForAssert, position) // FOR ... ASSERT ... IS NOT NULL
        case ConstraintVersion0 if !containsOn =>
          error(errorMessageForAssertExists, position) // FOR ... ASSERT EXISTS ...
        case ConstraintVersion1 if containsOn => error(errorMessageOnAssert, position) // ON ... ASSERT ... IS NOT NULL
        case ConstraintVersion0 if containsOn => error(errorMessageOnAssertExists, position) // ON ... ASSERT EXISTS ...
        case _ => checkOptionsMap("relationship property existence constraint", options) chain super.semanticCheck
      }
  }
}

case class DropRelationshipPropertyExistenceConstraint(
  variable: Variable,
  relType: RelTypeName,
  property: Property,
  useGraph: Option[GraphSelection] = None
)(val position: InputPosition) extends RelationshipPropertyConstraintCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)

  override def semanticCheck: SemanticCheck = error(
    "Relationship property existence constraints cannot be dropped by schema, please drop by name instead: DROP CONSTRAINT constraint_name. The constraint name can be found using SHOW CONSTRAINTS.",
    position
  )
}

case class DropConstraintOnName(name: String, ifExists: Boolean, useGraph: Option[GraphSelection] = None)(
  val position: InputPosition
) extends SchemaCommand {
  override def withGraph(useGraph: Option[UseGraph]): SchemaCommand = copy(useGraph = useGraph)(position)
  def semanticCheck = Seq()
}
