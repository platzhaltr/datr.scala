package org.platzhaltr

import org.scalatest._

import java.time.{Duration, LocalDate, LocalDateTime, ZoneOffset}
import org.threeten.extra.Interval

import Weekday._
import Month._

class EventSpec extends FlatSpec with Matchers {

  it should "interpret last <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // Friday

    LastWeekdayByName(Thursday).process(today)  shouldBe LocalDate.of(2014, 6, 26)
    LastWeekdayByName(Monday).process(today)    shouldBe LocalDate.of(2014, 6, 23)
    LastWeekdayByName(Tuesday).process(today)   shouldBe LocalDate.of(2014, 6, 24)
    LastWeekdayByName(Wednesday).process(today) shouldBe LocalDate.of(2014, 6, 25)
    LastWeekdayByName(Friday).process(today)    shouldBe LocalDate.of(2014, 6, 20)
    LastWeekdayByName(Saturday).process(today)  shouldBe LocalDate.of(2014, 6, 21)
    LastWeekdayByName(Sunday).process(today)    shouldBe LocalDate.of(2014, 6, 22)
  }

  it should "interpret next <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // Friday

    NextWeekdayByName(Monday).process(today)    shouldBe LocalDate.of(2014, 6, 30)
    NextWeekdayByName(Tuesday).process(today)   shouldBe LocalDate.of(2014, 7, 1)
    NextWeekdayByName(Wednesday).process(today) shouldBe LocalDate.of(2014, 7, 2)
    NextWeekdayByName(Thursday).process(today)  shouldBe LocalDate.of(2014, 7, 3)
    NextWeekdayByName(Friday).process(today)    shouldBe LocalDate.of(2014, 7, 4)
    NextWeekdayByName(Saturday).process(today)  shouldBe LocalDate.of(2014, 6, 28)
    NextWeekdayByName(Sunday).process(today)    shouldBe LocalDate.of(2014, 6, 29)
  }

  it should "interpret last <month>" in {
    val today = LocalDate.of(2014, 6, 1)

    LastMonthByName(January).process(today)   shouldBe LocalDate.of(2014, 1, 1)
    LastMonthByName(February).process(today)  shouldBe LocalDate.of(2014, 2, 1)
    LastMonthByName(March).process(today)     shouldBe LocalDate.of(2014, 3, 1)
    LastMonthByName(April).process(today)     shouldBe LocalDate.of(2014, 4, 1)
    LastMonthByName(May).process(today)       shouldBe LocalDate.of(2014, 5, 1)
    LastMonthByName(June).process(today)      shouldBe LocalDate.of(2013, 6, 1)
    LastMonthByName(July).process(today)      shouldBe LocalDate.of(2013, 7, 1)
    LastMonthByName(August).process(today)    shouldBe LocalDate.of(2013, 8, 1)
    LastMonthByName(September).process(today) shouldBe LocalDate.of(2013, 9, 1)
    LastMonthByName(October).process(today)   shouldBe LocalDate.of(2013, 10, 1)
    LastMonthByName(November).process(today)  shouldBe LocalDate.of(2013, 11, 1)
    LastMonthByName(December).process(today)  shouldBe LocalDate.of(2013, 12, 1)
  }

  it should "interpret next <month>" in {
    val today = LocalDate.of(2014, 1, 1)

    NextMonthByName(January).process(today)   shouldBe LocalDate.of(2015, 1, 1)
    NextMonthByName(February).process(today)  shouldBe LocalDate.of(2014, 2, 1)
    NextMonthByName(March).process(today)     shouldBe LocalDate.of(2014, 3, 1)
    NextMonthByName(April).process(today)     shouldBe LocalDate.of(2014, 4, 1)
    NextMonthByName(May).process(today)       shouldBe LocalDate.of(2014, 5, 1)
    NextMonthByName(June).process(today)      shouldBe LocalDate.of(2014, 6, 1)
    NextMonthByName(July).process(today)      shouldBe LocalDate.of(2014, 7, 1)
    NextMonthByName(August).process(today)    shouldBe LocalDate.of(2014, 8, 1)
    NextMonthByName(September).process(today) shouldBe LocalDate.of(2014, 9, 1)
    NextMonthByName(October).process(today)   shouldBe LocalDate.of(2014, 10, 1)
    NextMonthByName(November).process(today)  shouldBe LocalDate.of(2014, 11, 1)
    NextMonthByName(December).process(today)  shouldBe LocalDate.of(2014, 12, 1)
  }

  it should "interpret <count> <weekday> in <month>" in {
    val today = LocalDate.of(2014, 1, 1)

    WeekdayInMonth(1, Saturday, July).process(today) shouldBe LocalDate.of(2014, 7, 5)
    WeekdayInMonth(2, Saturday, July).process(today) shouldBe LocalDate.of(2014, 7, 12)
  }

  it should "interpret <hour>:<minute> if time in future" in {
    val now = LocalDateTime.of(2014, 1, 1, 14, 30)

    AtTime(Time(15,0)).process(now) shouldBe LocalDateTime.of(2014, 1, 1, 15, 0)
  }

  it should "interpret <hour>:<minute> if time in past" in {
    val now = LocalDateTime.of(2014, 1, 1, 14, 30)

    AtTime(Time(14,0)).process(now) shouldBe LocalDateTime.of(2014, 1, 2, 14, 0)
  }

  // Formal dates

  it should "interpret yyyy-mm-dd" in {
    val today = LocalDate.of(1970, 6, 20)

    OnDate(Date(1,1,Some(2014))).process(today) shouldBe LocalDate.of(2014, 1, 1)
  }

  // Durations

  it should "interpret 'for <n> seconds'" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 30, 30)

    ForSeconds(10).process(now) shouldBe Interval.of(now.toInstant(ZoneOffset.UTC), Duration.ofSeconds(10))
  }

  it should "interpret 'for <n> minutes'" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 30)

    ForMinutes(3).process(now) shouldBe Interval.of(now.toInstant(ZoneOffset.UTC), Duration.ofMinutes(3))
  }

  it should "interpret 'for <n> hours'" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 30)

    ForHours(6).process(now) shouldBe Interval.of(now.toInstant(ZoneOffset.UTC), Duration.ofHours(6))
  }

  it should "interpret 'for <n> days'" in {
    val today = LocalDate.of(1970, 6, 20)
    ForDays(3).process(today) shouldBe Interval.of(today.atStartOfDay.toInstant(ZoneOffset.UTC), Duration.ofDays(3))
  }

  it should "interpret 'for <n> days starting yesterday'" in {
    val today = LocalDate.of(1970, 6, 20)

    RelativeDateDuration(InDays(-1),ForDays(3)).process(today) shouldBe Interval.of(today.minusDays(1).atStartOfDay.toInstant(ZoneOffset.UTC), Duration.ofDays(3))
  }

  it should "interpret 'for <n> days starting today'" in {
    val today = LocalDate.of(1970, 6, 20)

    RelativeDateDuration(InDays(0),ForDays(3)).process(today) shouldBe Interval.of(today.atStartOfDay.toInstant(ZoneOffset.UTC), Duration.ofDays(3))
  }

  it should "interpret 'for <n> days starting tomorrow'" in {
    val today = LocalDate.of(1970, 6, 20)

    RelativeDateDuration(InDays(1),ForDays(3)).process(today) shouldBe Interval.of(today.plusDays(1).atStartOfDay.toInstant(ZoneOffset.UTC), Duration.ofDays(3))
  }

  it should "interpret 'for <n> days starting next monday'" in {
    val today = LocalDate.of(1970, 6, 20)
    val nextMonday = LocalDate.of(1970, 6, 22)

    RelativeDateDuration(NextWeekdayByName(Monday),ForDays(3)).process(today) shouldBe Interval.of(nextMonday.atStartOfDay.toInstant(ZoneOffset.UTC), Duration.ofDays(3))
  }

  it should "interpret 'from <time> [to|till|until] <time>'" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 0)
    val from = LocalDateTime.of(2014, 1, 1, 18, 30)
    val to = LocalDateTime.of(2014, 1, 1, 19, 0)

    FromTimeToTime(AtTime(Time(18,30)), AtTime(Time(19,0))).process(now) shouldBe Interval.of(from.toInstant(ZoneOffset.UTC), to.toInstant(ZoneOffset.UTC))
  }

  it should "interpret 'till <time>'" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 30)
    val next = LocalDateTime.of(2014, 1, 1, 19, 0)

    UntilTime(AtTime(Time(19,0))).process(now) shouldBe Interval.of(now.toInstant(ZoneOffset.UTC), next.toInstant(ZoneOffset.UTC))
  }

  it should "interpret 'till <time>', time before now" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 30)
    val next = LocalDateTime.of(2014, 1, 2, 17, 0)

    UntilTime(AtTime(Time(17,0))).process(now) shouldBe Interval.of(now.toInstant(ZoneOffset.UTC), next.toInstant(ZoneOffset.UTC))
  }

  it should "interpret 'till <weekday>'" in {
    val today = LocalDate.of(1970, 6, 20)
    val nextMonday = LocalDate.of(1970, 6, 22)

    val expected = Interval.of(today.atStartOfDay.toInstant(ZoneOffset.UTC), nextMonday.atStartOfDay.toInstant(ZoneOffset.UTC))

    RelativeDateDuration(InDays(0),UntilWeekday(Monday)).process(today) shouldBe expected
  }

  it should "interpret 'from <weekday> to <weekday>'" in {
    val today = LocalDate.of(1970, 6, 20)
    val nextMonday = LocalDate.of(1970, 6, 22)
    val fridayAfterMonday = LocalDate.of(1970, 6, 26)

    val expected = Interval.of(nextMonday.atStartOfDay.toInstant(ZoneOffset.UTC), fridayAfterMonday.atStartOfDay.toInstant(ZoneOffset.UTC))

    RelativeDateDuration(NextWeekdayByName(Monday),UntilWeekday(Friday)).process(today) shouldBe expected
  }

  // Combinations

  it should "interpret '<relative-day> <fuzzy-time>', e.g. 'tomorrow afternoon'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(1),AtTime(Time(16,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 2, 16, 0)
  }

  it should "interpret '<relative-day> <fuzzy-time>', e.g. 'yesterday evening'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(-1),AtTime(Time(19,0))).process(now)

    next shouldBe LocalDateTime.of(2013, 12, 31, 19, 0)
  }

    it should "interpret '<relative-day> <formal-time>', e.g. 'today at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(0),AtTime(Time(8,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 1, 8, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'tomorrow at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(1),AtTime(Time(8,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 2, 8, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'yesterday at 10 pm'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(-1),AtTime(Time(22,0))).process(now)

    next shouldBe LocalDateTime.of(2013, 12, 31, 22, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'in 2 days at 6 pm'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(2),AtTime(Time(18,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 3, 18, 0)
  }

  it should "interpret '<relative-weeks> <formal-time>', e.g. 'in 4 weeks at 12:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InWeeks(4),AtTime(Time(12,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 29, 12, 0)
  }

  it should "interpret '<relative-months> <formal-time>', e.g. 'in 3 months at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InMonths(3),AtTime(Time(8,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 4, 1, 8, 0)
  }

  it should "interpret '<relative-years> <formal-time>', e.g. 'in 1 year at 20:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InYears(1),AtTime(Time(20,0))).process(now)

    next shouldBe LocalDateTime.of(2015, 1, 1, 20, 0)
  }

  it should "interpret '<cardinal-weekday> <formal-time>', e.g. 'first wednesday of march at 22:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(WeekdayInMonth(1,Wednesday,March),AtTime(Time(22,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 3, 5, 22, 0)
  }

  it should "interpret '<weekday> <fuzzy-time>', e.g. 'saturday afternoon'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(NextWeekdayByName(Saturday),AtTime(Time(16,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 4, 16, 0)
  }

}
