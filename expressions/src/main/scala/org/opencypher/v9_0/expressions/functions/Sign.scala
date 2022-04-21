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
package org.opencypher.v9_0.expressions.functions

import org.opencypher.v9_0.expressions.TypeSignature
import org.opencypher.v9_0.util.symbols.CTFloat
import org.opencypher.v9_0.util.symbols.CTInteger

case object Sign extends Function {
  def name = "sign"

  override val signatures = Vector(
    TypeSignature(
      this,
      CTInteger,
      CTInteger,
      "Returns the signum of an integer number: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.",
      Category.NUMERIC
    ),
    TypeSignature(
      this,
      CTFloat,
      CTInteger,
      "Returns the signum of a floating point number: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.",
      Category.NUMERIC
    )
  )
}
