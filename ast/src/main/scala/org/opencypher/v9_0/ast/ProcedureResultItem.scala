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
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.ProcedureOutput
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.CypherType
import org.opencypher.v9_0.util.symbols.TypeSpec

object ProcedureResultItem {

  def apply(output: ProcedureOutput, variable: Variable)(position: InputPosition): ProcedureResultItem =
    ProcedureResultItem(Some(output), variable)(position)

  def apply(variable: Variable)(position: InputPosition): ProcedureResultItem =
    ProcedureResultItem(None, variable)(position)
}

case class ProcedureResultItem(output: Option[ProcedureOutput], variable: LogicalVariable)(val position: InputPosition)
    extends ASTNode with SemanticAnalysisTooling {

  val outputName: String = output.map(_.name).getOrElse(variable.name)

  def semanticCheck: SemanticCheck =
    // This is needed to prevent the initial round of semantic checking from failing with type errors
    // when procedure signatures have not yet been resolved
    declareVariable(variable, TypeSpec.all)

  def semanticCheck(types: Map[String, CypherType]): SemanticCheck =
    types
      .get(outputName)
      .map { typ => declareVariable(variable, typ.covariant): SemanticCheck }
      .getOrElse(error(s"Unknown procedure output: `$outputName`", position))
}
