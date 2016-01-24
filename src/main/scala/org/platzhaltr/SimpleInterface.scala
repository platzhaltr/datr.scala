package org.platzhaltr

import scala.util.{Failure, Success}

import java.time.{LocalDate,LocalDateTime}
import org.parboiled2.{ParseError, ErrorFormatter}

object SimpleInterface {
  val errorFmt = new ErrorFormatter(showTraces = true)

  def read(line: String): Unit = {
    val parser = new DateParser(line)
    parser.InputLine.run() match {
      case Success(result)        => s"Result: ${format(result)}"
      case Failure(e: ParseError) => s"Invalid expression: ${parser.formatError(e, errorFmt)}"
      case Failure(e)             => s"Unexpected error: ${e}"
    }
  }

  def format(result: ParsedCompound): String = {
    result match {
      case Left(Left(dateEvent)) =>
        val today = LocalDate.now
        s"${dateEvent.process(today)}"
      case Left(Right(timeEvent)) =>
        val now = LocalDateTime.now
        s"${timeEvent.process(now)}"
      case Right(Left(dateDuration)) =>
        val today = LocalDate.now
        s"${dateDuration.process(today)}"
      case Right(Right(timeDuration)) =>
        val now = LocalDateTime.now
        s"${timeDuration.process(now)}"
    }
  }
}
