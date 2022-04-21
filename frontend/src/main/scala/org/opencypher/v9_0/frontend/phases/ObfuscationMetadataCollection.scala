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
package org.opencypher.v9_0.frontend.phases

import org.opencypher.v9_0.ast.Statement
import org.opencypher.v9_0.expressions.Parameter
import org.opencypher.v9_0.expressions.SensitiveAutoParameter
import org.opencypher.v9_0.expressions.SensitiveLiteral
import org.opencypher.v9_0.expressions.SensitiveParameter
import org.opencypher.v9_0.frontend.phases.CompilationPhaseTracer.CompilationPhase.METADATA_COLLECTION
import org.opencypher.v9_0.util.Foldable.FoldableAny
import org.opencypher.v9_0.util.Foldable.SkipChildren
import org.opencypher.v9_0.util.LiteralOffset
import org.opencypher.v9_0.util.ObfuscationMetadata
import org.opencypher.v9_0.util.StepSequencer

/**
 * Collect sensitive literals and parameters.
 */
case object ObfuscationMetadataCollection extends Phase[BaseContext, BaseState, BaseState] {

  override def phase: CompilationPhaseTracer.CompilationPhase = METADATA_COLLECTION

  override def postConditions: Set[StepSequencer.Condition] = Set.empty

  override def process(from: BaseState, context: BaseContext): BaseState = {
    val extractedParamNames = from.maybeExtractedParams.map(_.keys.toSet).getOrElse(Set.empty)
    val preParserOffset = from.startPosition.map(_.offset).getOrElse(0)
    val parameters = from.statement().folder.findAllByClass[Parameter]

    val offsets = collectSensitiveLiteralOffsets(from.statement(), extractedParamNames, preParserOffset)
    val sensitiveParams = collectSensitiveParameterNames(parameters, extractedParamNames)

    from.withObfuscationMetadata(ObfuscationMetadata(offsets, sensitiveParams))
  }

  private def collectSensitiveLiteralOffsets(
    statement: Statement,
    extractedParamNames: Set[String],
    preParserOffset: Int
  ): Vector[LiteralOffset] =
    statement.folder.treeFold(Vector.empty[LiteralOffset]) {
      case literal: SensitiveLiteral =>
        acc => SkipChildren(acc :+ LiteralOffset(preParserOffset + literal.position.offset, literal.literalLength))
      case p: SensitiveAutoParameter if extractedParamNames.contains(p.name) =>
        acc => SkipChildren(acc :+ LiteralOffset(preParserOffset + p.position.offset, None))

    }.distinct.sortBy(_.start)

  private def collectSensitiveParameterNames(
    queryParams: Seq[Parameter],
    extractedParamNames: Set[String]
  ): Set[String] =
    queryParams.folder.findAllByClass[SensitiveParameter].map(_.name).toSet -- extractedParamNames
}
