/*
 * Copyright (C) 2009-2013 Mathias Doenitz, Alexander Myltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2.examples

import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scala.io.StdIn
import org.parboiled2._

object DateParser extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    // TODO: Replace next three lines with `scala.Predef.readLine(text: String, args: Any*)`
    // once BUG https://issues.scala-lang.org/browse/SI-8167 is fixed
    print("---\nEnter expression > ")
    Console.out.flush()

    StdIn.readLine() match {
      case "" =>
      case line =>
        val parser = new DateParser(line)
        parser.InputLine.run() match {
          case Success(result)        => println("Result: " + result)
          case Failure(e: ParseError) => println("Invalid expression: " + parser.formatError(e))
          case Failure(e)             => println("Unexpected error: " + e)
        }
        repl()
    }
  }
}

/**
 * This parser reads simple calculator expressions and evaluates them right during
 * the parsing run with the help of the value stack.
 */
class DateParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule {
    RelativeFuture
  }

  def RelativeFuture = rule { Next ~ Space ~ Weekday}

  def Next = rule { "next" }

  def Weekday: Rule1[Int]   = rule {Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday}

  def Monday: Rule1[Int]    = rule {capture(ignoreCase("monday")    | ignoreCase("mon") ) ~> ((s: String) => 1)}
  def Tuesday: Rule1[Int]   = rule {capture(ignoreCase("tuesday")   | ignoreCase("tue") ) ~> ((s: String) => 2)}
  def Wednesday: Rule1[Int] = rule {capture(ignoreCase("wednesday") | ignoreCase("wed") ) ~> ((s: String) => 3)}
  def Thursday: Rule1[Int]  = rule {capture(ignoreCase("thursday")  | ignoreCase("thu") ) ~> ((s: String) => 4)}
  def Friday: Rule1[Int]    = rule {capture(ignoreCase("friday")    | ignoreCase("fri") ) ~> ((s: String) => 5)}
  def Saturday: Rule1[Int]  = rule {capture(ignoreCase("saturday")  | ignoreCase("sat") ) ~> ((s: String) => 6)}
  def Sunday: Rule1[Int]    = rule {capture(ignoreCase("sunday")    | ignoreCase("sun") ) ~> ((s: String) => 7)}

  def Space = rule { zeroOrMore(" ") }
}
