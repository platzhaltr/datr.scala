package com.acme

import scala.util.{Failure, Success}
import org.scalatest._
import org.scalatest.Matchers._

import org.parboiled2.ParseError

import Weekday._
import Month._

class DateParserSpec extends fixture.FreeSpec with Matchers {

  type FixtureParam = TestData

  override def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(test))
  }

  // Formal dates

  "2014-10-23" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  "20141023" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  "2014/10/23" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  "23/10/2014" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  "on 2014-10-23" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  "on 20141023" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  "on 2014/10/23" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  "on 23/10/2014" in { td =>
    parse(td.name) shouldBe Left(OnDate(Date(2014,10,23)))
  }

  // Relaxed dates

  "today" in { td =>
    parse(td.name) shouldBe Left(InDays(0))
  }

  "tomorrow" in { td =>
    parse(td.name) shouldBe Left(InDays(1))
  }

  "yesterday" in { td =>
    parse(td.name) shouldBe Left(InDays(-1))
  }

  "saturday" in { td =>
    parse(td.name) shouldBe Left(NextWeekdayByName(Saturday))
  }


  "next monday" in { td =>
    parse(td.name) shouldBe Left(NextWeekdayByName(Monday))
  }

  "last monday" in { td =>
    parse(td.name) shouldBe Left(LastWeekdayByName(Monday))
  }

  "last tuesday" in { td =>
    parse(td.name) shouldBe Left(LastWeekdayByName(Tuesday))
  }

  "next tuesday" in { td =>
    parse(td.name) shouldBe Left(NextWeekdayByName(Tuesday))
  }

  "last january" in { td =>
    parse(td.name) shouldBe Left(LastMonthByName(January))
  }

  "next february" in { td =>
    parse(td.name) shouldBe Left(NextMonthByName(February))
  }

  "last week" in { td =>
    parse(td.name) shouldBe Left(InWeeks(-1))
  }

  "next week" in { td =>
    parse(td.name) shouldBe Left(InWeeks(1))
  }

  "last month" in { td =>
    parse(td.name) shouldBe Left(InMonths(-1))
  }

  "next month" in { td =>
    parse(td.name) shouldBe Left(InMonths(1))
  }

  "last year" in { td =>
    parse(td.name) shouldBe Left(InYears(-1))
  }

  "next year" in { td =>
    parse(td.name) shouldBe Left(InYears(1))
  }

  "second saturday in september" in { td =>
    parse(td.name) shouldBe Left(WeekdayInMonth(2, Saturday, September))
  }

  "in 1 day" in { td =>
    parse(td.name) shouldBe Left(InDays(1))
  }

  "in 2 weeks" in { td =>
    parse(td.name) shouldBe Left(InWeeks(2))
  }

  "in 3 months" in { td =>
    parse(td.name) shouldBe Left(InMonths(3))
  }

  "in 4 years" in { td =>
    parse(td.name) shouldBe Left(InYears(4))
  }

  "one day ago" in { td =>
    parse(td.name) shouldBe Left(InDays(-1))
  }

  "two weeks ago" in { td =>
    parse(td.name) shouldBe Left(InWeeks(-2))
  }

  "three months ago" in { td =>
    parse(td.name) shouldBe Left(InMonths(-3))
  }

  "four years ago" in { td =>
    parse(td.name) shouldBe Left(InYears(-4))
  }

  // Formal times

  "5:00" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(5,0)))
  }

  "at 2:00" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(2,0)))
  }

  "6pm" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(18,0)))
  }

  "5:30 a.m." in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(5,30)))
  }

  "12:15 am" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(0,15)))
  }

  "12:15 pm" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(12,15)))
  }

  "at 7 pm" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(19,0)))
  }

  // fuzzy times

  "noon" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(12,0)))
  }

  "afternoon" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(16,0)))
  }

  "evening" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(19,0)))
  }

  "midnight" in { td =>
    parse(td.name) shouldBe Right(AtTime(Time(0,0)))
  }

  // relaxed times

  "10 seconds ago" in { td =>
    parse(td.name) shouldBe Right(InSeconds(-10))
  }

  "in 5 minutes" in { td =>
    parse(td.name) shouldBe Right(InMinutes(5))
  }

  "4 hours from now" in { td =>
    parse(td.name) shouldBe Right(InHours(4))
  }

  // combinations

  "tomorrow afternoon" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InDays(1),AtTime(Time(16,0))))
  }

  "yesterday evening" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InDays(-1),AtTime(Time(19,0))))
  }

  "tomorrow at 8 a.m." in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InDays(1),AtTime(Time(8,0))))
  }

  "yesterday at 10 pm" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InDays(-1),AtTime(Time(22,0))))
  }

  "in 2 days at 6 pm" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InDays(2),AtTime(Time(18,0))))
  }

  "in 4 weeks at 12:00" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InWeeks(4),AtTime(Time(12,0))))
  }

  "in 3 months at 8 a.m" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InMonths(3),AtTime(Time(8,0))))
  }

  "in 1 year at 20:00" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(InYears(1),AtTime(Time(20,0))))
  }

  "first wednesday of march at 22:00" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(WeekdayInMonth(1,Wednesday,March),AtTime(Time(22,0))))
  }

  "saturday afternoon" in { td =>
    parse(td.name) shouldBe Right(DateTimeEvent(NextWeekdayByName(Saturday),AtTime(Time(16,0))))
  }

  def parse(line: String): Either[DateEvent, TimeEvent] = {
    val parser = new DateParser(line)
    parser.InputLine.run() match {
      case Success(result) => result
      case Failure(e: ParseError) =>
        println(s"Invalid expression: ${parser.formatError(e, showTraces = true)}")
        throw new IllegalArgumentException(e)
      case Failure(e)             => throw new IllegalArgumentException(e)
    }
  }

}
