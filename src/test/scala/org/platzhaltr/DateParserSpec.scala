package org.platzhaltr

import scala.util.{Failure, Success}
import org.scalatest._
import org.scalatest.Matchers._

import org.parboiled2.{ParseError, ErrorFormatter}

import java.time.{DayOfWeek, Month}
import java.time.DayOfWeek._
import java.time.Month._

class DateParserSpec extends fixture.FreeSpec with Matchers {

  type FixtureParam = TestData

  override def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(test))
  }

  // Formal dates
  private def onDate(month: Int, day: Int, year: Option[Int] = None) = {
    OnDate(Date(month,day,year))
  }

  "2014-10-23" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "20141023" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "2014/10/23" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "23/10/2014" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "on 2014-10-23" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "on 20141023" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "on 2014/10/23" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "on 23/10/2014" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "23. October" in { td =>
    parse(td.name) shouldBe onDate(10,23)
  }

  "on 23. Oct 2014" in { td =>
    parse(td.name) shouldBe onDate(10,23,Some(2014))
  }

  "on 1st August" in { td =>
    parse(td.name) shouldBe onDate(8,1)
  }

  "on 2nd August 2015" in { td =>
    parse(td.name) shouldBe onDate(8,2,Some(2015))
  }

  "on 3rd Sep 2015" in { td =>
    parse(td.name) shouldBe onDate(9,3,Some(2015))
  }

  "on 4th Dec 2015" in { td =>
    parse(td.name) shouldBe onDate(12,4,Some(2015))
  }

  "on 5th August 2015" in { td =>
    parse(td.name) shouldBe onDate(8,5,Some(2015))
  }

  // Relaxed dates

  private def inDays(days: Int) = {
    InDays(days)
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

  private def nextWeekendByName(weekday: DayOfWeek) = {
    NextWeekdayByName(weekday)
  }

  "mon." in { td =>
    parse(td.name) shouldBe nextWeekendByName(MONDAY)
  }

  "saturday" in { td =>
    parse(td.name) shouldBe nextWeekendByName(SATURDAY)
  }

  "next monday" in { td =>
    parse(td.name) shouldBe nextWeekendByName(MONDAY)
  }

  "next tuesday" in { td =>
    parse(td.name) shouldBe nextWeekendByName(TUESDAY)
  }

  private def lastWeekendByName(weekday: DayOfWeek) = {
    LastWeekdayByName(weekday)
  }

  "last monday" in { td =>
    parse(td.name) shouldBe lastWeekendByName(MONDAY)
  }

  "last tuesday" in { td =>
    parse(td.name) shouldBe lastWeekendByName(TUESDAY)
  }

  "last january" in { td =>
    parse(td.name) shouldBe LastMonthByName(JANUARY)
  }

  "last jan." in { td =>
    parse(td.name) shouldBe LastMonthByName(JANUARY)
  }

  "january" in { td =>
    parse(td.name) shouldBe NextMonthByName(JANUARY)
  }

  "apr." in { td =>
    parse(td.name) shouldBe NextMonthByName(APRIL)
  }

  "in dec." in { td =>
    parse(td.name) shouldBe NextMonthByName(DECEMBER)
  }

  "next february" in { td =>
    parse(td.name) shouldBe NextMonthByName(FEBRUARY)
  }

  "last week" in { td =>
    parse(td.name) shouldBe InWeeks(-1)
  }

  "next week" in { td =>
    parse(td.name) shouldBe InWeeks(1)
  }

  "last month" in { td =>
    parse(td.name) shouldBe InMonths(-1)
  }

  "next month" in { td =>
    parse(td.name) shouldBe InMonths(1)
  }

  "last year" in { td =>
    parse(td.name) shouldBe InYears(-1)
  }

  "next year" in { td =>
    parse(td.name) shouldBe InYears(1)
  }

  "second saturday in september" in { td =>
    parse(td.name) shouldBe WeekdayInMonth(2, SATURDAY, SEPTEMBER)
  }

  "in 1 day" in { td =>
    parse(td.name) shouldBe InDays(1)
  }

  "in one day" in { td =>
    parse(td.name) shouldBe InDays(1)
  }

  "in 2 weeks" in { td =>
    parse(td.name) shouldBe InWeeks(2)
  }

  "in two weeks" in { td =>
    parse(td.name) shouldBe InWeeks(2)
  }

  "in 3 months" in { td =>
    parse(td.name) shouldBe InMonths(3)
  }

  "in three months" in { td =>
    parse(td.name) shouldBe InMonths(3)
  }

  "in 4 years" in { td =>
    parse(td.name) shouldBe InYears(4)
  }

  "in four years" in { td =>
    parse(td.name) shouldBe InYears(4)
  }

  "one day ago" in { td =>
    parse(td.name) shouldBe InDays(-1)
  }

  "two weeks ago" in { td =>
    parse(td.name) shouldBe InWeeks(-2)
  }

  "three months ago" in { td =>
    parse(td.name) shouldBe InMonths(-3)
  }

  "four years ago" in { td =>
    parse(td.name) shouldBe InYears(-4)
  }

  // Formal times

  private def atTime(hours: Int, minutes: Int) = {
    AtTime(Time(hours,minutes))
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
    parse(td.name) shouldBe InSeconds(-10)
  }

  "in 5 minutes" in { td =>
    parse(td.name) shouldBe InMinutes(5)
  }

  "4 hours from now" in { td =>
    parse(td.name) shouldBe InHours(4)
  }

  // durations

  "for 10 seconds" in { td =>
    parse(td.name) shouldBe ForSeconds(10)
  }

  "for 3 minutes" in { td =>
    parse(td.name) shouldBe ForMinutes(3)
  }

  "for 6 hours" in { td =>
    parse(td.name) shouldBe ForHours(6)
  }

  "for 3 days" in { td =>
    parse(td.name) shouldBe ForDays(3)
  }

  "for 3 days starting yesterday" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(InDays(-1),ForDays(3))
  }

  "for 3 days starting today" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(InDays(0),ForDays(3))
  }

  "for 3 days starting tomorrow" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(InDays(1),ForDays(3))
  }

  "for 3 days starting next monday" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(NextWeekdayByName(MONDAY),ForDays(3))
  }

  "for 8 days starting in 5 weeks" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(InWeeks(5),ForDays(8))
  }

  "for 8 days starting second saturday in april" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(WeekdayInMonth(2, SATURDAY, APRIL),ForDays(8))
  }

  "from 9:30 to 12:15" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(Time(9,30)), AtTime(Time(12,15)))
  }

  "from 9:30 till 12:15" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(Time(9,30)), AtTime(Time(12,15)))
  }

  "from 9:30 until 12:15" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(Time(9,30)), AtTime(Time(12,15)))
  }

  "till 12:15" in { td =>
    parse(td.name) shouldBe UntilTime(AtTime(Time(12,15)))
  }

  "until 7 pm" in { td =>
    parse(td.name) shouldBe UntilTime(AtTime(Time(19,0)))
  }

  "till friday" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(InDays(0),UntilWeekday(FRIDAY))
  }

  "until friday" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(InDays(0),UntilWeekday(FRIDAY))
  }

  "from monday to friday" in { td =>
    parse(td.name) shouldBe RelativeDateDuration(NextWeekdayByName(MONDAY),UntilWeekday(FRIDAY))
  }

  // combinations

  private def dateTimeEvent (dateEvent: DateEvent, timeEvent: TimeEvent) = {
    DateTimeEvent(dateEvent,timeEvent)
  }

  private def inDaysAtTime (days: Int)(hours: Int, minutes: Int) = {
    dateTimeEvent(InDays(days),AtTime(Time(hours,minutes)))
  }

  private def onDateTime(month: Int, day: Int, hours: Int, minutes: Int, year: Option[Int] = None) = {
    dateTimeEvent(OnDate(Date(month, day, year)),AtTime(Time(hours,minutes)))
  }

  "23. July 12:00" in { td =>
    parse(td.name) shouldBe onDateTime(7,23,12,0)
  }

  "on 1st August 2015 5 a.m." in { td =>
    parse(td.name) shouldBe onDateTime(8,1,5,0, Some(2015))
  }

  "on 24. Dec 8 pm" in { td =>
    parse(td.name) shouldBe onDateTime(12,24,20,0)
  }

  "on 24. Dec at 8 pm" in { td =>
    parse(td.name) shouldBe onDateTime(12,24,20,0)
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

  "monday 12:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(NextWeekdayByName(MONDAY), AtTime(Time(12,0)))
  }

  "tuesday at 5 a.m." in { td =>
    parse(td.name) shouldBe dateTimeEvent(NextWeekdayByName(TUESDAY), AtTime(Time(5,0)))
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
    parse(td.name) shouldBe dateTimeEvent(WeekdayInMonth(1,WEDNESDAY,MARCH),AtTime(Time(22,0)))
  }

  "saturday afternoon" in { td =>
    parse(td.name) shouldBe dateTimeEvent(NextWeekdayByName(SATURDAY),AtTime(Time(16,0)))
  }

  def parse(line: String): ParseResult = {
    val parser = new DateParser(line)
    parser.InputLine.run() match {
      case Success(result) => result
      case Failure(e: ParseError) =>
        println(s"Invalid expression: ${parser.formatError(e, new ErrorFormatter(showTraces = true))}")
        throw new IllegalArgumentException(e)
      case Failure(e)             => throw new IllegalArgumentException(e)
    }
  }

}
