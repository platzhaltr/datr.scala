package org.platzhaltr.datr

import scala.util.{Failure, Success}
import org.scalatest._

import org.parboiled2.{ParseError, ErrorFormatter}

import java.time.{DayOfWeek, LocalDate, LocalTime, MonthDay}
import java.time.DayOfWeek._
import java.time.Month._

class DateParserSpec extends fixture.FreeSpec with Matchers {

  type FixtureParam = TestData

  override def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(test))
  }

  // Formal dates
  private def onDate(month: Int, day: Int) = {
    OnDate(Left(MonthDay.of(month, day)))
  }
  private def onDate(month: Int, day: Int, year: Int) = {
    OnDate(Right(LocalDate.of(year, month, day)))
  }

  "2014-10-23" in { td =>
    parse(td.name) shouldBe onDate(10,23, 2014)
  }

  "20141023" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "2014/10/23" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "23/10/2014" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "on 2014-10-23" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "on 20141023" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "on 2014/10/23" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "on 23/10/2014" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "23. October" in { td =>
    parse(td.name) shouldBe onDate(10,23)
  }

  "on 23. Oct 2014" in { td =>
    parse(td.name) shouldBe onDate(10,23,2014)
  }

  "on 1st August" in { td =>
    parse(td.name) shouldBe onDate(8,1)
  }

  "first march" in { td =>
    parse(td.name) shouldBe onDate(3,1)
  }

  "the 2nd of april" in { td =>
    parse(td.name) shouldBe onDate(4,2)
  }

  "on 2nd August 2015" in { td =>
    parse(td.name) shouldBe onDate(8, 2, 2015)
  }

  "on 3rd Sep 2015" in { td =>
    parse(td.name) shouldBe onDate(9,3, 2015)
  }

  "on 4th Dec 2015" in { td =>
    parse(td.name) shouldBe onDate(12, 4, 2015)
  }

  "on 5th August 2015" in { td =>
    parse(td.name) shouldBe onDate(8, 5, 2015)
  }

  // Relaxed dates

  private def inDays(days: Int) = {
    InDays(days)
  }

  "today" in { td =>
    parse(td.name) shouldBe inDays(0)
  }

  "tom" in { td =>
    parse(td.name) shouldBe inDays(1)
  }

  "tomorrow" in { td =>
    parse(td.name) shouldBe inDays(1)
  }

  "yesterday" in { td =>
    parse(td.name) shouldBe inDays(-1)
  }

  private def nextWeekdayByName(weekday: DayOfWeek) = {
    NextWeekdayByName(weekday)
  }

  "mon." in { td =>
    parse(td.name) shouldBe nextWeekdayByName(MONDAY)
  }

  "saturday" in { td =>
    parse(td.name) shouldBe nextWeekdayByName(SATURDAY)
  }

  "next monday" in { td =>
    parse(td.name) shouldBe nextWeekdayByName(MONDAY)
  }

  "next tuesday" in { td =>
    parse(td.name) shouldBe nextWeekdayByName(TUESDAY)
  }

  "next week tuesday" in { td =>
    parse(td.name) shouldBe NextWeekWeekdayByName(TUESDAY)
  }

  private def lastWeekdayByName(weekday: DayOfWeek) = {
    LastWeekdayByName(weekday)
  }

  "last monday" in { td =>
    parse(td.name) shouldBe lastWeekdayByName(MONDAY)
  }

  "last tuesday" in { td =>
    parse(td.name) shouldBe lastWeekdayByName(TUESDAY)
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

  "second sat in sep" in { td =>
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
    AtTime(LocalTime.of(hours,minutes))
  }

  "5" in { td =>
    parse(td.name) shouldBe atTime(5,0)
  }

  "5:00" in { td =>
    parse(td.name) shouldBe atTime(5,0)
  }

  "at 2:00" in { td =>
    parse(td.name) shouldBe atTime(2,0)
  }

  "at 20:00" in { td =>
    parse(td.name) shouldBe atTime(20,0)
  }

  "@ 3:00" in { td =>
    parse(td.name) shouldBe atTime(3,0)
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

  "now" in { td =>
    parse(td.name) shouldBe InSeconds(0)
  }

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

  "from 5 to 7" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(LocalTime.of(5,0)), AtTime(LocalTime.of(7,0)))
  }

  "from 9:30 to 12:15" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(LocalTime.of(9,30)), AtTime(LocalTime.of(12,15)))
  }

  "from 9:30 till 12:15" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(LocalTime.of(9,30)), AtTime(LocalTime.of(12,15)))
  }

  "from 9:30 until 12:15" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(LocalTime.of(9,30)), AtTime(LocalTime.of(12,15)))
  }

  "from 9:30 until 12:15 next sunday" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(LocalTime.of(9,30)), AtTime(LocalTime.of(12,15)), Some(NextWeekdayByName(SUNDAY)))
  }

  "from 14 to 15 on mon" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(LocalTime.of(14,0)), AtTime(LocalTime.of(15,0)), Some(NextWeekdayByName(MONDAY)))
  }

  "from 14 to 15 next mon" in { td =>
    parse(td.name) shouldBe FromTimeToTime(AtTime(LocalTime.of(14,0)), AtTime(LocalTime.of(15,0)), Some(NextWeekdayByName(MONDAY)))
  }

  "till 12:15" in { td =>
    parse(td.name) shouldBe UntilTime(AtTime(LocalTime.of(12,15)))
  }

  "until 7 pm" in { td =>
    parse(td.name) shouldBe UntilTime(AtTime(LocalTime.of(19,0)))
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
    dateTimeEvent(InDays(days),AtTime(LocalTime.of(hours,minutes)))
  }

  private def onDateTime(month: Int, day: Int, hours: Int, minutes: Int, year: Option[Int] = None) = {
    val date = year match {
      case None => onDate(month, day)
      case Some(y) => onDate(month, day, y)
    }
    dateTimeEvent(date, AtTime(LocalTime.of(hours,minutes)))
  }

  "23. July 12:00" in { td =>
    parse(td.name) shouldBe onDateTime(7,23,12,0)
  }

  "on 1st August 2015 5 a.m." in { td =>
    parse(td.name) shouldBe onDateTime(8,1,5,0,Some(2015))
  }

  "on 24. Dec 8 pm" in { td =>
    parse(td.name) shouldBe onDateTime(12,24,20,0)
  }

  "on 24. Dec 20" in { td =>
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
    parse(td.name) shouldBe dateTimeEvent(NextWeekdayByName(MONDAY), AtTime(LocalTime.of(12,0)))
  }

  "tuesday at 5 a.m." in { td =>
    parse(td.name) shouldBe dateTimeEvent(NextWeekdayByName(TUESDAY), AtTime(LocalTime.of(5,0)))
  }

  "in 2 days at 6 pm" in { td =>
    parse(td.name) shouldBe inDaysAtTime(2)(18,0)
  }

  "in 4 weeks at 12:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(InWeeks(4),AtTime(LocalTime.of(12,0)))
  }

  "in 6 weeks at 18:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(InWeeks(6),AtTime(LocalTime.of(18,0)))
  }

  "in 3 months at 8 a.m" in { td =>
    parse(td.name) shouldBe dateTimeEvent(InMonths(3),AtTime(LocalTime.of(8,0)))
  }

  "in 1 year at 20:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(InYears(1),AtTime(LocalTime.of(20,0)))
  }

  "next week tue 5" in { td =>
    parse(td.name) shouldBe dateTimeEvent(NextWeekWeekdayByName(TUESDAY),AtTime(LocalTime.of(5,0)))
  }

  "first wednesday of march at 22:00" in { td =>
    parse(td.name) shouldBe dateTimeEvent(WeekdayInMonth(1,WEDNESDAY,MARCH),AtTime(LocalTime.of(22,0)))
  }

  "saturday afternoon" in { td =>
    parse(td.name) shouldBe dateTimeEvent(NextWeekdayByName(SATURDAY),AtTime(LocalTime.of(16,0)))
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
