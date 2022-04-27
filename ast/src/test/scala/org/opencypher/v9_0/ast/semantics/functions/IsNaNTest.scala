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

import org.opencypher.v9_0.util.symbols.CTBoolean
import org.opencypher.v9_0.util.symbols.CTFloat
import org.opencypher.v9_0.util.symbols.CTInteger
import org.opencypher.v9_0.util.symbols.CTNode

class IsNaNTest extends FunctionTestBase("isNaN") {

  test("shouldFailIfWrongArguments") {
    testInvalidApplication()("Insufficient parameters for function 'isNaN'")
    testInvalidApplication(CTInteger, CTInteger)("Too many parameters for function 'isNaN'")
    testInvalidApplication(CTFloat, CTFloat)("Too many parameters for function 'isNaN'")
  }

  test("shouldHandleAllSpecializations") {
    testValidTypes(CTInteger)(CTBoolean)
    testValidTypes(CTFloat)(CTBoolean)
  }

  test("shouldHandleCombinedSpecializations") {
    testValidTypes(CTFloat | CTInteger)(CTBoolean)
  }

  test("shouldReturnErrorIfInvalidArgumentTypes") {
    testInvalidApplication(CTNode)("Type mismatch: expected Float or Integer but was Node")
  }
}
