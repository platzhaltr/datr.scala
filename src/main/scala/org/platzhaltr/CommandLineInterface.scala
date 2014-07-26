package org.platzhaltr

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success}

import org.joda.time.{LocalDate,LocalDateTime}
import org.parboiled2.ParseError

object CommandLineInterface extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    StdIn.readLine("---\nEnter expression > ") match {
      case "" =>
      case line =>
        val parser = new DateParser(line)
        parser.InputLine.run() match {
          case Success(result) =>
            result match {
              case Left(Left(dateEvent)) =>
                val today = new LocalDate
                println(s"Result: ${dateEvent.process(today)}")
              case Left(Right(timeEvent)) =>
                val now = new LocalDateTime
                println(s"Result: ${timeEvent.process(now)}")
              case Right(Left(dateDuration)) =>
                val today = new LocalDate
                println(s"Result: ${dateDuration.process(today)}")
              case Right(Right(timeDuration)) =>
                val now = new LocalDateTime
                println(s"Result: ${timeDuration.process(now)}")
            }
          case Failure(e: ParseError) => println(s"Invalid expression: ${parser.formatError(e, showTraces = true)}")
          case Failure(e)             => println(s"Unexpected error: ${e}")
        }
        repl()
    }
  }
}
