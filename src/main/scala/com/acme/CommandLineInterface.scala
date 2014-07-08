package com.acme

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success}

import org.joda.time.{LocalDate,LocalDateTime}
import org.parboiled2.ParseError

object CommandLineInterface extends App {
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
          case Success(result)        =>
            val now = new LocalDateTime
            val today = new LocalDate

            result match {
              case e @ LastWeekdayByName(_) => println(s"Result: ${e.process(today)}")
              case e @ NextWeekdayByName(_) => println(s"Result: ${e.process(today)}")
              case e @ LastMonthByName(_) => println(s"Result: ${e.process(today)}")
              case e @ NextMonthByName(_) => println(s"Result: ${e.process(today)}")

              case e @ InDays(_) => println(s"Result: ${e.process(today)}")
              case e @ InMonths(_) => println(s"Result: ${e.process(today)}")
              case e @ InYears(_) => println(s"Result: ${e.process(today)}")
              case e @ WeekdayInMonth(_,_,_) => println(s"Result: ${e.process(today)}")

              case e @ AtTime(_) => println(s"Result: ${e.process(now)}")
            }
          case Failure(e: ParseError) => println(s"Invalid expression: ${parser.formatError(e, showTraces = true)}")
          case Failure(e)             => println(s"Unexpected error: ${e}")
        }
        repl()
    }
  }
}
