package org.platzhaltr

import scala.util.{Failure, Success}

import java.time.{LocalDate,LocalDateTime, ZoneId}
import org.parboiled2.{ParseError, ErrorFormatter}

import java.time.format.DateTimeFormatter

object Cli extends App {

  private val UTC = ZoneId.of("UTC")
  // 01.03. 10:00
  private val KhalStartFormatter =  DateTimeFormatter.ofPattern("dd.MM. HH:mm").withZone(UTC)
  private val KhalEndFormatter =  DateTimeFormatter.ofPattern("HH:mm").withZone(UTC)

  private val ErrorFmt = new ErrorFormatter(showTraces = true)

  private val parser = new DateParser(args.mkString(" "))
  parser.InputLine.run() match {
    case Success(result)        =>
      result match {
        case dateEvent: DateEvent =>
          val today = LocalDate.now
          println(s"${today.`with`(dateEvent)}")
        case timeEvent: TimeEvent =>
          val now = LocalDateTime.now
          println(s"${timeEvent.process(now)}")
        case dateDuration: DateDuration =>
          val today = LocalDate.now
          println(s"${dateDuration.process(today)}")
        case timeDuration: TimeDuration =>
          val now = LocalDateTime.now
          val interval = timeDuration.process(now)
          val start = KhalStartFormatter.format(interval.getStart)
          val end = KhalEndFormatter.format(interval.getEnd)

          println(s"$start $end")
      }
    case Failure(e: ParseError) =>
      println(s"Invalid expression: ${parser.formatError(e, ErrorFmt)}")
      System.exit(1)
    case Failure(e)             =>
      println(s"Unexpected error: $e")
      System.exit(1)
  }
}
