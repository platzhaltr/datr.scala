package org.platzhaltr

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success}

import java.time.{LocalDate,LocalDateTime}
import org.parboiled2.{ParseError, ErrorFormatter}

object Cli extends App {

  private val ErrorFmt = new ErrorFormatter(showTraces = true)

  val parser = new DateParser(args.mkString(" "))

  parser.InputLine.run() match {
    case Success(result)        =>
      result match {
        case dateEvent: DateEvent =>
          val today = LocalDate.now
          println(s"${dateEvent.process(today)}")
        case timeEvent: TimeEvent =>
          val now = LocalDateTime.now
          println(s"${timeEvent.process(now)}")
        case dateDuration: DateDuration =>
          val today = LocalDate.now
          println(s"${dateDuration.process(today)}")
        case timeDuration: TimeDuration =>
          val now = LocalDateTime.now
          println(s"${timeDuration.process(now)}")
      }
    case Failure(e: ParseError) =>
      println(s"Invalid expression: ${parser.formatError(e, ErrorFmt)}")
      System.exit(1)
    case Failure(e)             =>
      println(s"Unexpected error: ${e}")
      System.exit(1)
  }
}
