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

import org.opencypher.v9_0.ast.CatalogName.quote
import org.opencypher.v9_0.ast.CatalogName.separatorChar
import org.opencypher.v9_0.ast.CatalogName.separatorString

object CatalogName {

  def apply(head: String, tail: List[String]): CatalogName = {
    CatalogName(head :: tail)
  }

  def apply(parts: String*): CatalogName = {
    CatalogName(parts.head, parts.tail.toList)
  }

  val separatorChar: Char = '.'
  val separatorString: String = separatorChar.toString
  val quoteChar = "`"

  def quote(str: String): String = quoteChar ++ str ++ quoteChar
}

/**
 * A qualified graph name is used in a Cypher query to address a specific graph in the catalog.
 */
case class CatalogName(parts: List[String]) {

  def qualifiedNameString: String =
    parts
      .map(part => if (part.contains(separatorChar)) quote(part) else part)
      .mkString(separatorString)
}
