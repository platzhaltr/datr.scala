package org.platzhaltr

import scala.util.{Failure, Success}

import java.time.{LocalDate, LocalDateTime}
import org.parboiled2.{ParseError, ErrorFormatter}

object SimpleInterface {
  val errorFmt = new ErrorFormatter(showTraces = true)

  def read(line: String): String = {
    val parser = new DateParser(line)
    parser.InputLine.run() match {
      case Success(result)        => s"Result: ${format(result)}"
      case Failure(e: ParseError) => s"Invalid expression: ${parser.formatError(e, errorFmt)}"
      case Failure(e)             => s"Unexpected error: $e"
    }
  }

  def format(result: ParseResult): String = {
    result match {
      case dateEvent: DateEvent =>
        val today = LocalDate.now
        s"${today.`with`(dateEvent)}"
      case timeEvent: TimeEvent =>
        val now = LocalDateTime.now
        s"${now.`with`(timeEvent)}"
      case dateDuration: DateDuration =>
        val today = LocalDate.now
        s"${dateDuration.process(today)}"
      case timeDuration: TimeDuration =>
        val now = LocalDateTime.now
        s"${timeDuration.process(now)}"
    }
  }
}
