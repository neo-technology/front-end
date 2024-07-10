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
package org.opencypher.v9_0.ast.factory.neo4j

import org.opencypher.v9_0.parser.javacc.ParseException
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

import scala.util.Failure
import scala.util.Success
import scala.util.Try

trait JavaccParserTestBase[T, J] extends CypherFunSuite {

  type Extra

  def convert(astNode: T): J

  def convert(astNode: T, extra: Extra): J = convert(astNode)

  case class ResultCheck(actuals: Seq[J], text: String) {

    def or(other: ResultCheck): ResultCheck = copy(actuals = actuals ++ other.actuals)

    def shouldGive(expected: J): Unit = {
      actuals foreach {
        actual =>
          actual should equal(expected)
      }
    }

    def shouldGive(expected: InputPosition => J): Unit = {
      shouldGive(expected(InputPosition(0, 0, 0)))
    }

    def shouldMatch(expected: PartialFunction[J, Unit]): Unit = {
      actuals foreach {
        actual => expected.isDefinedAt(actual) should equal(true)
      }
    }

    def shouldVerify(expected: J => Unit): Unit = {
      actuals foreach expected
    }

    override def toString: String = s"ResultCheck( $text -> $actuals )"
  }

  def parsing(s: String)(implicit p: JavaccRule[T]): ResultCheck = convertResult(parseRule(p, s), None, s)

  def parsingWith(s: String, extra: Extra)(implicit p: JavaccRule[T]): ResultCheck =
    convertResult(parseRule(p, s), Some(extra), s)

  def partiallyParsing(s: String)(implicit p: JavaccRule[T]): ResultCheck = convertResult(parseRule(p, s), None, s)

  def assertFails(s: String)(implicit p: JavaccRule[T]): Unit = {
    parseRule(p, s).toOption match {
      case None        =>
      case Some(thing) => fail(s"'$s' should not have been parsed correctly, parsed as $thing")
    }
  }

  def assertFailsWithException(s: String, expected: Exception)(implicit p: JavaccRule[T]): Unit = {
    parseRule(p, s) match {
      case Failure(exception) =>
        exception.getClass should be(expected.getClass)
        exception.getMessage shouldBe fixLineSeparator(expected.getMessage)
      case Success(thing) => fail(s"'$s' should not have been parsed correctly, parsed as $thing")
    }
  }

  def assertFailsWithMessage(s: String, expectedMessage: String)(implicit p: JavaccRule[T]): Unit = {
    parseRule(p, s) match {
      case Failure(exception) =>
        exception.getMessage shouldBe fixLineSeparator(expectedMessage)
      case Success(thing) => fail(s"'$s' should not have been parsed correctly, parsed as $thing")
    }
  }

  def assertFailsWithMessageStart(s: String, expectedMessage: String)(implicit p: JavaccRule[T]): Unit = {
    parseRule(p, s) match {
      case Failure(exception) =>
        exception.getMessage should startWith(fixLineSeparator(expectedMessage))
      case Success(thing) => fail(s"'$s' should not have been parsed correctly, parsed as $thing")
    }
  }

  private def fixLineSeparator(message: String): String = {
    // This is needed because current version of scala seems to produce \n from multi line strings
    // This produces a problem with windows line endings \r\n
    if (message.contains(System.lineSeparator()))
      message
    else
      message.replaceAll("\n", System.lineSeparator())
  }

  protected def parseRule(rule: JavaccRule[T], queryText: String): Try[T] = Try(rule(queryText))

  private def convertResult(r: Try[T], extra: Option[Extra], input: String) = r match {
    case Success(t) =>
      val converted = extra match {
        case None    => convert(t)
        case Some(e) => convert(t, e)
      }
      ResultCheck(Seq(converted), input)

    case Failure(exception) =>
      println(exception.getMessage)
      fail(generateErrorMessage(input, exception))
  }

  private def generateErrorMessage(input: String, exception: Throwable): String = {
    val defaultMessage = s"'$input' failed with: $exception"
    exception match {
      case e: ParseException =>
        if (input.contains("\n")) {
          defaultMessage
        } else {
          val pos = e.currentToken.beginOffset
          val indentation = " ".repeat(pos)
          s"""
             |'$input'
             | $indentation^
             |failed with $exception""".stripMargin
        }
      case _ => defaultMessage
    }
  }
}
