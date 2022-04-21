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
package org.opencypher.v9_0.ast.semantics.functions

import org.opencypher.v9_0.util.symbols.CTAny
import org.opencypher.v9_0.util.symbols.CTBoolean
import org.opencypher.v9_0.util.symbols.CTDate
import org.opencypher.v9_0.util.symbols.CTFloat
import org.opencypher.v9_0.util.symbols.CTInteger
import org.opencypher.v9_0.util.symbols.CTList
import org.opencypher.v9_0.util.symbols.CTNode
import org.opencypher.v9_0.util.symbols.CTNumber
import org.opencypher.v9_0.util.symbols.CTString

class ToIntegerTest extends FunctionTestBase("toInteger") {

  test("shouldAcceptCorrectTypes") {
    testValidTypes(CTString)(CTInteger)
    testValidTypes(CTFloat)(CTInteger)
    testValidTypes(CTInteger)(CTInteger)
    testValidTypes(CTNumber.covariant)(CTInteger)
    testValidTypes(CTAny.covariant)(CTInteger)
    testValidTypes(CTBoolean)(CTInteger)
  }

  // Currently we coerce CTList to boolean. This is going away and when it does we should reinstate this test
  ignore("shouldFailTypeCheckForIncompatibleListArgument") {
    testInvalidApplication(CTList(CTAny).covariant)(
      "Type mismatch: expected Boolean, Float, Integer, Number or String but was List<T>"
    )
  }

  test("shouldFailTypeCheckForIncompatibleArguments") {
    testInvalidApplication(CTNode)(
      "Type mismatch: expected Boolean, Float, Integer, Number or String but was Node"
    )

    testInvalidApplication(CTDate)(
      "Type mismatch: expected Boolean, Float, Integer, Number or String but was Date"
    )
  }

  test("shouldFailIfWrongNumberOfArguments") {
    testInvalidApplication()(
      "Insufficient parameters for function 'toInteger'"
    )
    testInvalidApplication(CTString, CTString)(
      "Too many parameters for function 'toInteger'"
    )
  }
}
