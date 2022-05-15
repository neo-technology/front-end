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
package org.opencypher.v9_0.ast.semantics

import scala.annotation.tailrec

object SemanticCheckInterpreter {

  def runCheck(check: SemanticCheck, initialState: SemanticState): SemanticCheckResult = {
    run(SemanticCheckResult.success(initialState), List(ExecutableCheck(check)))
  }

  @tailrec
  private def run(result: SemanticCheckResult,
                  checkStack: List[ExecutableCheck],
                 ): SemanticCheckResult = {

    checkStack match {
      case Nil =>
        result

      case ExecutableCheck.Wrapper(check) :: checkStackTail =>
        check match {
          case SemanticCheck.Leaf(f) =>
            run(f(result.state), checkStackTail)

          case SemanticCheck.Map(check, func) =>
            run(result, ExecutableCheck(check) :: ExecutableCheck.Map(func) :: checkStackTail)

          case SemanticCheck.FlatMap(check, func) =>
            run(result, ExecutableCheck(check) :: ExecutableCheck.FlatMap(func) :: checkStackTail)
        }

      case ExecutableCheck.Map(func) :: checkStackTail =>
        run(func(result), checkStackTail)

      case ExecutableCheck.FlatMap(func) :: checkStackTail =>
        run(result, ExecutableCheck(func(result)) :: checkStackTail)
    }
  }

  private sealed trait ExecutableCheck
  private object ExecutableCheck {
    def apply(check: SemanticCheck): ExecutableCheck = Wrapper(check)

    final case class Map(f: SemanticCheckResult => SemanticCheckResult) extends ExecutableCheck
    final case class FlatMap(f: SemanticCheckResult => SemanticCheck) extends ExecutableCheck
    final case class Wrapper(check: SemanticCheck) extends ExecutableCheck
  }
}
