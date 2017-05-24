package org.platzhaltr

import org.scalatest._

import java.time._
import java.time.DayOfWeek._
import java.time.Month._
import org.threeten.extra.Interval

import Month._

class EventSpec extends FlatSpec with Matchers {

  it should "interpret last <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // FRIDAY

    LastWeekdayByName(MONDAY).process(today)    shouldBe LocalDate.of(2014, 6, 23)
    LastWeekdayByName(TUESDAY).process(today)   shouldBe LocalDate.of(2014, 6, 24)
    LastWeekdayByName(WEDNESDAY).process(today) shouldBe LocalDate.of(2014, 6, 25)
    LastWeekdayByName(THURSDAY).process(today)  shouldBe LocalDate.of(2014, 6, 26)
    LastWeekdayByName(FRIDAY).process(today)    shouldBe LocalDate.of(2014, 6, 20)
    LastWeekdayByName(SATURDAY).process(today)  shouldBe LocalDate.of(2014, 6, 21)
    LastWeekdayByName(SUNDAY).process(today)    shouldBe LocalDate.of(2014, 6, 22)
  }

  it should "interpret next <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // FRIDAY

    NextWeekdayByName(MONDAY).process(today)    shouldBe LocalDate.of(2014, 6, 30)
    NextWeekdayByName(TUESDAY).process(today)   shouldBe LocalDate.of(2014, 7, 1)
    NextWeekdayByName(WEDNESDAY).process(today) shouldBe LocalDate.of(2014, 7, 2)
    NextWeekdayByName(THURSDAY).process(today)  shouldBe LocalDate.of(2014, 7, 3)
    NextWeekdayByName(FRIDAY).process(today)    shouldBe LocalDate.of(2014, 7, 4)
    NextWeekdayByName(SATURDAY).process(today)  shouldBe LocalDate.of(2014, 6, 28)
    NextWeekdayByName(SUNDAY).process(today)    shouldBe LocalDate.of(2014, 6, 29)
  }

  it should "interpret next week <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // FRIDAY

    NextWeekWeekdayByName(MONDAY).process(today)    shouldBe LocalDate.of(2014, 6, 30)
    NextWeekWeekdayByName(TUESDAY).process(today)   shouldBe LocalDate.of(2014, 7, 1)
    NextWeekWeekdayByName(WEDNESDAY).process(today) shouldBe LocalDate.of(2014, 7, 2)
    NextWeekWeekdayByName(THURSDAY).process(today)  shouldBe LocalDate.of(2014, 7, 3)
    NextWeekWeekdayByName(FRIDAY).process(today)    shouldBe LocalDate.of(2014, 7, 4)
    NextWeekWeekdayByName(SATURDAY).process(today)  shouldBe LocalDate.of(2014, 7, 5)
    NextWeekWeekdayByName(SUNDAY).process(today)    shouldBe LocalDate.of(2014, 7, 6)
  }

  it should "interpret last <month>" in {
    val today = LocalDate.of(2014, 6, 1)

    LastMonthByName(JANUARY).process(today)   shouldBe LocalDate.of(2014, 1, 1)
    LastMonthByName(FEBRUARY).process(today)  shouldBe LocalDate.of(2014, 2, 1)
    LastMonthByName(MARCH).process(today)     shouldBe LocalDate.of(2014, 3, 1)
    LastMonthByName(APRIL).process(today)     shouldBe LocalDate.of(2014, 4, 1)
    LastMonthByName(MAY).process(today)       shouldBe LocalDate.of(2014, 5, 1)
    LastMonthByName(JUNE).process(today)      shouldBe LocalDate.of(2013, 6, 1)
    LastMonthByName(JULY).process(today)      shouldBe LocalDate.of(2013, 7, 1)
    LastMonthByName(AUGUST).process(today)    shouldBe LocalDate.of(2013, 8, 1)
    LastMonthByName(SEPTEMBER).process(today) shouldBe LocalDate.of(2013, 9, 1)
    LastMonthByName(OCTOBER).process(today)   shouldBe LocalDate.of(2013, 10, 1)
    LastMonthByName(NOVEMBER).process(today)  shouldBe LocalDate.of(2013, 11, 1)
    LastMonthByName(DECEMBER).process(today)  shouldBe LocalDate.of(2013, 12, 1)
  }

  it should "interpret next <month>" in {
    val today = LocalDate.of(2014, 1, 1)

    NextMonthByName(JANUARY).process(today)   shouldBe LocalDate.of(2015, 1, 1)
    NextMonthByName(FEBRUARY).process(today)  shouldBe LocalDate.of(2014, 2, 1)
    NextMonthByName(MARCH).process(today)     shouldBe LocalDate.of(2014, 3, 1)
    NextMonthByName(APRIL).process(today)     shouldBe LocalDate.of(2014, 4, 1)
    NextMonthByName(MAY).process(today)       shouldBe LocalDate.of(2014, 5, 1)
    NextMonthByName(JUNE).process(today)      shouldBe LocalDate.of(2014, 6, 1)
    NextMonthByName(JULY).process(today)      shouldBe LocalDate.of(2014, 7, 1)
    NextMonthByName(AUGUST).process(today)    shouldBe LocalDate.of(2014, 8, 1)
    NextMonthByName(SEPTEMBER).process(today) shouldBe LocalDate.of(2014, 9, 1)
    NextMonthByName(OCTOBER).process(today)   shouldBe LocalDate.of(2014, 10, 1)
    NextMonthByName(NOVEMBER).process(today)  shouldBe LocalDate.of(2014, 11, 1)
    NextMonthByName(DECEMBER).process(today)  shouldBe LocalDate.of(2014, 12, 1)
  }

  it should "interpret <count> <weekday> in <month>" in {
    val today = LocalDate.of(2014, 1, 1)

    WeekdayInMonth(1, SATURDAY, JULY).process(today) shouldBe LocalDate.of(2014, 7, 5)
    WeekdayInMonth(2, SATURDAY, JULY).process(today) shouldBe LocalDate.of(2014, 7, 12)
  }

  it should "interpret <hour>:<minute> if time in future" in {
    val now = LocalDateTime.of(2014, 1, 1, 14, 30)

    AtTime(LocalTime.of(15,0)).process(now) shouldBe LocalDateTime.of(2014, 1, 1, 15, 0)
  }

  it should "interpret <hour>:<minute> if time in past" in {
    val now = LocalDateTime.of(2014, 1, 1, 14, 30)

    AtTime(LocalTime.of(14,0)).process(now) shouldBe LocalDateTime.of(2014, 1, 2, 14, 0)
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
    val nextMONDAY = LocalDate.of(1970, 6, 22)

    RelativeDateDuration(NextWeekdayByName(MONDAY),ForDays(3)).process(today) shouldBe Interval.of(nextMONDAY.atStartOfDay.toInstant(ZoneOffset.UTC), Duration.ofDays(3))
  }

  it should "interpret 'from <time> [to|till|until] <time>'" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 0)
    val from = LocalDateTime.of(2014, 1, 1, 18, 30)
    val to = LocalDateTime.of(2014, 1, 1, 19, 0)

    FromTimeToTime(AtTime(LocalTime.of(18,30)), AtTime(LocalTime.of(19,0))).process(now) shouldBe Interval.of(from.toInstant(ZoneOffset.UTC), to.toInstant(ZoneOffset.UTC))
  }

  it should "interpret 'till <time>'" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 30)
    val next = LocalDateTime.of(2014, 1, 1, 19, 0)

    UntilTime(AtTime(LocalTime.of(19,0))).process(now) shouldBe Interval.of(now.toInstant(ZoneOffset.UTC), next.toInstant(ZoneOffset.UTC))
  }

  it should "interpret 'till <time>', time before now" in {
    val now = LocalDateTime.of(2014, 1, 1, 18, 30)
    val next = LocalDateTime.of(2014, 1, 2, 17, 0)

    UntilTime(AtTime(LocalTime.of(17,0))).process(now) shouldBe Interval.of(now.toInstant(ZoneOffset.UTC), next.toInstant(ZoneOffset.UTC))
  }

  it should "interpret 'till <weekday>'" in {
    val today = LocalDate.of(1970, 6, 20)
    val nextMONDAY = LocalDate.of(1970, 6, 22)

    val expected = Interval.of(today.atStartOfDay.toInstant(ZoneOffset.UTC), nextMONDAY.atStartOfDay.toInstant(ZoneOffset.UTC))

    RelativeDateDuration(InDays(0),UntilWeekday(MONDAY)).process(today) shouldBe expected
  }

  it should "interpret 'from <weekday> to <weekday>'" in {
    val today = LocalDate.of(1970, 6, 20)
    val nextMONDAY = LocalDate.of(1970, 6, 22)
    val fridayAfterMONDAY = LocalDate.of(1970, 6, 26)

    val expected = Interval.of(nextMONDAY.atStartOfDay.toInstant(ZoneOffset.UTC), fridayAfterMONDAY.atStartOfDay.toInstant(ZoneOffset.UTC))

    RelativeDateDuration(NextWeekdayByName(MONDAY),UntilWeekday(FRIDAY)).process(today) shouldBe expected
  }

  // Combinations

  it should "interpret '<relative-day> <fuzzy-time>', e.g. 'tomorrow afternoon'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(1),AtTime(LocalTime.of(16,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 2, 16, 0)
  }

  it should "interpret '<relative-day> <fuzzy-time>', e.g. 'yesterday evening'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(-1),AtTime(LocalTime.of(19,0))).process(now)

    next shouldBe LocalDateTime.of(2013, 12, 31, 19, 0)
  }

    it should "interpret '<relative-day> <formal-time>', e.g. 'today at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(0),AtTime(LocalTime.of(8,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 1, 8, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'tomorrow at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(1),AtTime(LocalTime.of(8,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 2, 8, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'yesterday at 10 pm'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(-1),AtTime(LocalTime.of(22,0))).process(now)

    next shouldBe LocalDateTime.of(2013, 12, 31, 22, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'in 2 days at 6 pm'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InDays(2),AtTime(LocalTime.of(18,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 3, 18, 0)
  }

  it should "interpret '<relative-weeks> <formal-time>', e.g. 'in 4 weeks at 12:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InWeeks(4),AtTime(LocalTime.of(12,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 29, 12, 0)
  }

  it should "interpret '<relative-months> <formal-time>', e.g. 'in 3 months at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InMonths(3),AtTime(LocalTime.of(8,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 4, 1, 8, 0)
  }

  it should "interpret '<relative-years> <formal-time>', e.g. 'in 1 year at 20:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(InYears(1),AtTime(LocalTime.of(20,0))).process(now)

    next shouldBe LocalDateTime.of(2015, 1, 1, 20, 0)
  }

  it should "interpret '<cardinal-weekday> <formal-time>', e.g. 'first wednesday of march at 22:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(WeekdayInMonth(1,WEDNESDAY,MARCH),AtTime(LocalTime.of(22,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 3, 5, 22, 0)
  }

  it should "interpret '<weekday> <fuzzy-time>', e.g. 'saturday afternoon'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = DateTimeEvent(NextWeekdayByName(SATURDAY),AtTime(LocalTime.of(16,0))).process(now)

    next shouldBe LocalDateTime.of(2014, 1, 4, 16, 0)
  }

}
