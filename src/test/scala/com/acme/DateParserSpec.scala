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
  private def date(year: Int, month: Int, day: Int) = {
    Left(Left(OnDate(Date(year,month,day))))
  }

  "2014-10-23" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  "20141023" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  "2014/10/23" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  "23/10/2014" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  "on 2014-10-23" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  "on 20141023" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  "on 2014/10/23" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  "on 23/10/2014" in { td =>
    parse(td.name) shouldBe date(2014,10,23)
  }

  // Relaxed dates

  private def inDays(days: Int) = {
    Left(Left(InDays(days)))
  }

  "today" in { td =>
    parse(td.name) shouldBe inDays(0)
  }

  "tomorrow" in { td =>
    parse(td.name) shouldBe inDays(1)
  }

  "yesterday" in { td =>
    parse(td.name) shouldBe inDays(-1)
  }

  private def nextWeekendByName(weekday: Weekday) = {
    Left(Left(NextWeekdayByName(weekday)))
  }

  "saturday" in { td =>
    parse(td.name) shouldBe nextWeekendByName(Saturday)
  }

  "next monday" in { td =>
    parse(td.name) shouldBe nextWeekendByName(Monday)
  }

  "next tuesday" in { td =>
    parse(td.name) shouldBe nextWeekendByName(Tuesday)
  }

  private def lastWeekendByName(weekday: Weekday) = {
    Left(Left(LastWeekdayByName(weekday)))
  }

  "last monday" in { td =>
    parse(td.name) shouldBe lastWeekendByName(Monday)
  }

  "last tuesday" in { td =>
    parse(td.name) shouldBe lastWeekendByName(Tuesday)
  }

  "last january" in { td =>
    parse(td.name) shouldBe Left(Left(LastMonthByName(January)))
  }

  "next february" in { td =>
    parse(td.name) shouldBe Left(Left(NextMonthByName(February)))
  }

  "last week" in { td =>
    parse(td.name) shouldBe Left(Left(InWeeks(-1)))
  }

  "next week" in { td =>
    parse(td.name) shouldBe Left(Left(InWeeks(1)))
  }

  "last month" in { td =>
    parse(td.name) shouldBe Left(Left(InMonths(-1)))
  }

  "next month" in { td =>
    parse(td.name) shouldBe Left(Left(InMonths(1)))
  }

  "last year" in { td =>
    parse(td.name) shouldBe Left(Left(InYears(-1)))
  }

  "next year" in { td =>
    parse(td.name) shouldBe Left(Left(InYears(1)))
  }

  "second saturday in september" in { td =>
    parse(td.name) shouldBe Left(Left(WeekdayInMonth(2, Saturday, September)))
  }

  "in 1 day" in { td =>
    parse(td.name) shouldBe Left(Left(InDays(1)))
  }

  "in 2 weeks" in { td =>
    parse(td.name) shouldBe Left(Left(InWeeks(2)))
  }

  "in 3 months" in { td =>
    parse(td.name) shouldBe Left(Left(InMonths(3)))
  }

  "in 4 years" in { td =>
    parse(td.name) shouldBe Left(Left(InYears(4)))
  }

  "one day ago" in { td =>
    parse(td.name) shouldBe Left(Left(InDays(-1)))
  }

  "two weeks ago" in { td =>
    parse(td.name) shouldBe Left(Left(InWeeks(-2)))
  }

  "three months ago" in { td =>
    parse(td.name) shouldBe Left(Left(InMonths(-3)))
  }

  "four years ago" in { td =>
    parse(td.name) shouldBe Left(Left(InYears(-4)))
  }

  // Formal times

  private def atTime(hours: Int, minutes: Int) = {
    Left(Right(AtTime(Time(hours,minutes))))
  }

  "5:00" in { td =>
    parse(td.name) shouldBe atTime(5,0)
  }

  "at 2:00" in { td =>
    parse(td.name) shouldBe atTime(2,0)
  }

  "6pm" in { td =>
    parse(td.name) shouldBe atTime(18,0)
  }

  "5:30 a.m." in { td =>
    parse(td.name) shouldBe atTime(5,30)
  }

  "12:15 am" in { td =>
    parse(td.name) shouldBe atTime(0,15)
  }

  "12:15 pm" in { td =>
    parse(td.name) shouldBe atTime(12,15)
  }

  "at 7 pm" in { td =>
    parse(td.name) shouldBe atTime(19,0)
  }

  // fuzzy times

  "noon" in { td =>
    parse(td.name) shouldBe atTime(12,0)
  }

  "afternoon" in { td =>
    parse(td.name) shouldBe atTime(16,0)
  }

  "evening" in { td =>
    parse(td.name) shouldBe atTime(19,0)
  }

  "midnight" in { td =>
    parse(td.name) shouldBe atTime(0,0)
  }

  // relaxed times

  "10 seconds ago" in { td =>
    parse(td.name) shouldBe Left(Right(InSeconds(-10)))
  }

  "in 5 minutes" in { td =>
    parse(td.name) shouldBe Left(Right(InMinutes(5)))
  }

  "4 hours from now" in { td =>
    parse(td.name) shouldBe Left(Right(InHours(4)))
  }

  // durations
  "for 6 hours" in { td =>
    parse(td.name) shouldBe Right(ForHours(6))
  }

  // combinations

  private def dateTimeEvent (dateEvent: DateEvent, timeEvent: TimeEvent) = {
    Left(Right(DateTimeEvent(dateEvent,timeEvent)))
  }

  private def inDaysAtTime (days: Int)(hours: Int, minutes: Int) = {
    dateTimeEvent(InDays(days),AtTime(Time(hours,minutes)))
  }

  "tomorrow afternoon" in { td =>
    parse(td.name) shouldBe inDaysAtTime(1)(16,0)
  }

  "yesterday evening" in { td =>
    parse(td.name) shouldBe inDaysAtTime(-1)(19,0)
  }

  "tomorrow at 8 a.m." in { td =>
    parse(td.name) shouldBe inDaysAtTime(1)(8,0)
  }

  "yesterday at 10 pm" in { td =>
    parse(td.name) shouldBe inDaysAtTime(-1)(22,0)
  }

  "in 2 days at 6 pm" in { td =>
    parse(td.name) shouldBe inDaysAtTime(2)(18,0)
  }

  "in 4 weeks at 12:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(InWeeks(4),AtTime(Time(12,0)))
  }

  "in 3 months at 8 a.m" in { td =>
    parse(td.name) shouldBe dateTimeEvent(InMonths(3),AtTime(Time(8,0)))
  }

  "in 1 year at 20:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(InYears(1),AtTime(Time(20,0)))
  }

  "first wednesday of march at 22:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(WeekdayInMonth(1,Wednesday,March),AtTime(Time(22,0)))
  }

  "saturday afternoon" in { td =>
    parse(td.name) shouldBe dateTimeEvent(NextWeekdayByName(Saturday),AtTime(Time(16,0)))
  }

  def parse(line: String): CompoundEvent = {
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
