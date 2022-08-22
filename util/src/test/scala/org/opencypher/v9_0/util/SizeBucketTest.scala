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
package org.opencypher.v9_0.util

import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class SizeBucketTest extends CypherFunSuite {

  test("test computeBucket") {
    SizeBucket.computeBucket(0) shouldEqual 0
    SizeBucket.computeBucket(1) shouldEqual 1
    SizeBucket.computeBucket(7) shouldEqual 10
    SizeBucket.computeBucket(10) shouldEqual 10
    SizeBucket.computeBucket(17) shouldEqual 100
    SizeBucket.computeBucket(42) shouldEqual 100
    SizeBucket.computeBucket(1001) shouldEqual 10000
  }
}
