package org.platzhaltr

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success}

import org.joda.time.{LocalDate,LocalDateTime}
import org.parboiled2.{ParseError, ErrorFormatter}

object CommandLineInterface extends App {
  val errorFormatter = new ErrorFormatter(showTraces = true)

  repl()

  @tailrec
  def repl(): Unit = {
    StdIn.readLine("---\nEnter expression > ") match {
      case "" =>
      case line =>
        val parser = new DateParser(line)
        parser.InputLine.run() match {
          case Success(result) => println(s"Result: ${format(result)}")
          case Failure(e: ParseError) =>
            println(s"Invalid expression: ${parser.formatError(e, errorFormatter)}")
          case Failure(e)             => println(s"Unexpected error: ${e}")
        }
        repl()
    }
  }

  def format(result: ParsedCompound): String = {
    result match {
      case Left(Left(dateEvent)) =>
        val today = new LocalDate
        s"${dateEvent.process(today)}"
      case Left(Right(timeEvent)) =>
        val now = new LocalDateTime
        s"${timeEvent.process(now)}"
      case Right(Left(dateDuration)) =>
        val today = new LocalDate
        s"${dateDuration.process(today)}"
      case Right(Right(timeDuration)) =>
        val now = new LocalDateTime
        s"${timeDuration.process(now)}"
    }
  }
}
