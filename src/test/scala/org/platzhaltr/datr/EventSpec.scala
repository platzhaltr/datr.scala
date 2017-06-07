package org.platzhaltr.datr

import org.scalatest._

import java.time._
import java.time.DayOfWeek._
import org.threeten.extra.Interval

import Month._

class EventSpec extends FlatSpec with Matchers {

  it should "interpret last <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // FRIDAY

    today.`with`(LastWeekdayByName(MONDAY))    shouldBe LocalDate.of(2014, 6, 23)
    today.`with`(LastWeekdayByName(TUESDAY))   shouldBe LocalDate.of(2014, 6, 24)
    today.`with`(LastWeekdayByName(WEDNESDAY)) shouldBe LocalDate.of(2014, 6, 25)
    today.`with`(LastWeekdayByName(THURSDAY))  shouldBe LocalDate.of(2014, 6, 26)
    today.`with`(LastWeekdayByName(FRIDAY))    shouldBe LocalDate.of(2014, 6, 20)
    today.`with`(LastWeekdayByName(SATURDAY))  shouldBe LocalDate.of(2014, 6, 21)
    today.`with`(LastWeekdayByName(SUNDAY))    shouldBe LocalDate.of(2014, 6, 22)
  }

  it should "interpret next <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // FRIDAY

    today.`with`(NextWeekdayByName(MONDAY))    shouldBe LocalDate.of(2014, 6, 30)
    today.`with`(NextWeekdayByName(TUESDAY))   shouldBe LocalDate.of(2014, 7, 1)
    today.`with`(NextWeekdayByName(WEDNESDAY)) shouldBe LocalDate.of(2014, 7, 2)
    today.`with`(NextWeekdayByName(THURSDAY))  shouldBe LocalDate.of(2014, 7, 3)
    today.`with`(NextWeekdayByName(FRIDAY))    shouldBe LocalDate.of(2014, 7, 4)
    today.`with`(NextWeekdayByName(SATURDAY))  shouldBe LocalDate.of(2014, 6, 28)
    today.`with`(NextWeekdayByName(SUNDAY))    shouldBe LocalDate.of(2014, 6, 29)
  }

  it should "interpret next week <weekday>" in {
    val today = LocalDate.of(2014, 6, 27) // FRIDAY

    today.`with`(NextWeekWeekdayByName(MONDAY))    shouldBe LocalDate.of(2014, 6, 30)
    today.`with`(NextWeekWeekdayByName(TUESDAY))   shouldBe LocalDate.of(2014, 7, 1)
    today.`with`(NextWeekWeekdayByName(WEDNESDAY)) shouldBe LocalDate.of(2014, 7, 2)
    today.`with`(NextWeekWeekdayByName(THURSDAY))  shouldBe LocalDate.of(2014, 7, 3)
    today.`with`(NextWeekWeekdayByName(FRIDAY))    shouldBe LocalDate.of(2014, 7, 4)
    today.`with`(NextWeekWeekdayByName(SATURDAY))  shouldBe LocalDate.of(2014, 7, 5)
    today.`with`(NextWeekWeekdayByName(SUNDAY))    shouldBe LocalDate.of(2014, 7, 6)
  }

  it should "interpret last <month>" in {
    val today = LocalDate.of(2014, 6, 1)

    today.`with`(LastMonthByName(JANUARY))   shouldBe LocalDate.of(2014, 1, 1)
    today.`with`(LastMonthByName(FEBRUARY))  shouldBe LocalDate.of(2014, 2, 1)
    today.`with`(LastMonthByName(MARCH))     shouldBe LocalDate.of(2014, 3, 1)
    today.`with`(LastMonthByName(APRIL))     shouldBe LocalDate.of(2014, 4, 1)
    today.`with`(LastMonthByName(MAY))       shouldBe LocalDate.of(2014, 5, 1)
    today.`with`(LastMonthByName(JUNE))      shouldBe LocalDate.of(2013, 6, 1)
    today.`with`(LastMonthByName(JULY))      shouldBe LocalDate.of(2013, 7, 1)
    today.`with`(LastMonthByName(AUGUST))    shouldBe LocalDate.of(2013, 8, 1)
    today.`with`(LastMonthByName(SEPTEMBER)) shouldBe LocalDate.of(2013, 9, 1)
    today.`with`(LastMonthByName(OCTOBER))   shouldBe LocalDate.of(2013, 10, 1)
    today.`with`(LastMonthByName(NOVEMBER))  shouldBe LocalDate.of(2013, 11, 1)
    today.`with`(LastMonthByName(DECEMBER))  shouldBe LocalDate.of(2013, 12, 1)
  }

  it should "interpret next <month>" in {
    val today = LocalDate.of(2014, 1, 1)

    today.`with`(NextMonthByName(JANUARY))   shouldBe LocalDate.of(2015, 1, 1)
    today.`with`(NextMonthByName(FEBRUARY))  shouldBe LocalDate.of(2014, 2, 1)
    today.`with`(NextMonthByName(MARCH))     shouldBe LocalDate.of(2014, 3, 1)
    today.`with`(NextMonthByName(APRIL))     shouldBe LocalDate.of(2014, 4, 1)
    today.`with`(NextMonthByName(MAY))       shouldBe LocalDate.of(2014, 5, 1)
    today.`with`(NextMonthByName(JUNE))      shouldBe LocalDate.of(2014, 6, 1)
    today.`with`(NextMonthByName(JULY))      shouldBe LocalDate.of(2014, 7, 1)
    today.`with`(NextMonthByName(AUGUST))    shouldBe LocalDate.of(2014, 8, 1)
    today.`with`(NextMonthByName(SEPTEMBER)) shouldBe LocalDate.of(2014, 9, 1)
    today.`with`(NextMonthByName(OCTOBER))   shouldBe LocalDate.of(2014, 10, 1)
    today.`with`(NextMonthByName(NOVEMBER))  shouldBe LocalDate.of(2014, 11, 1)
    today.`with`(NextMonthByName(DECEMBER))  shouldBe LocalDate.of(2014, 12, 1)
  }

  it should "interpret <count> <weekday> in <month>" in {
    val today = LocalDate.of(2014, 1, 1)

    today.`with`(WeekdayInMonth(1, SATURDAY, JULY)) shouldBe LocalDate.of(2014, 7, 5)
    today.`with`(WeekdayInMonth(2, SATURDAY, JULY)) shouldBe LocalDate.of(2014, 7, 12)
  }

  it should "interpret <hour>:<minute> if time in future" in {
    val now = LocalDateTime.of(2014, 1, 1, 14, 30)

    now.`with`(AtTime(LocalTime.of(15,0))) shouldBe LocalDateTime.of(2014, 1, 1, 15, 0)
  }

  it should "interpret <hour>:<minute> if time in past" in {
    val now = LocalDateTime.of(2014, 1, 1, 14, 30)

    now.`with`(AtTime(LocalTime.of(14,0))) shouldBe LocalDateTime.of(2014, 1, 2, 14, 0)
  }

  // Formal dates

  it should "interpret yyyy-mm-dd" in {
    val today = LocalDate.of(1970, 6, 20)

    today.`with`(OnDate(Right(LocalDate.of(2014, 1, 1)))) shouldBe LocalDate.of(2014, 1, 1)
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
    val next = now.`with`(DateTimeEvent(InDays(1),AtTime(LocalTime.of(16,0))))

    next shouldBe LocalDateTime.of(2014, 1, 2, 16, 0)
  }

  it should "interpret '<relative-day> <fuzzy-time>', e.g. 'yesterday evening'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InDays(-1),AtTime(LocalTime.of(19,0))))

    next shouldBe LocalDateTime.of(2013, 12, 31, 19, 0)
  }

    it should "interpret '<relative-day> <formal-time>', e.g. 'today at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InDays(0),AtTime(LocalTime.of(8,0))))

    next shouldBe LocalDateTime.of(2014, 1, 1, 8, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'tomorrow at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InDays(1),AtTime(LocalTime.of(8,0))))

    next shouldBe LocalDateTime.of(2014, 1, 2, 8, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'yesterday at 10 pm'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InDays(-1),AtTime(LocalTime.of(22,0))))

    next shouldBe LocalDateTime.of(2013, 12, 31, 22, 0)
  }

  it should "interpret '<relative-day> <formal-time>', e.g. 'in 2 days at 6 pm'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InDays(2),AtTime(LocalTime.of(18,0))))

    next shouldBe LocalDateTime.of(2014, 1, 3, 18, 0)
  }

  it should "interpret '<relative-weeks> <formal-time>', e.g. 'in 4 weeks at 12:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InWeeks(4),AtTime(LocalTime.of(12,0))))

    next shouldBe LocalDateTime.of(2014, 1, 29, 12, 0)
  }

  it should "interpret '<relative-months> <formal-time>', e.g. 'in 3 months at 8 a.m.'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InMonths(3),AtTime(LocalTime.of(8,0))))

    next shouldBe LocalDateTime.of(2014, 4, 1, 8, 0)
  }

  it should "interpret '<relative-years> <formal-time>', e.g. 'in 1 year at 20:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(InYears(1),AtTime(LocalTime.of(20,0))))

    next shouldBe LocalDateTime.of(2015, 1, 1, 20, 0)
  }

  it should "interpret '<cardinal-weekday> <formal-time>', e.g. 'first wednesday of march at 22:00'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(WeekdayInMonth(1,WEDNESDAY,MARCH),AtTime(LocalTime.of(22,0))))

    next shouldBe LocalDateTime.of(2014, 3, 5, 22, 0)
  }

  it should "interpret '<weekday> <fuzzy-time>', e.g. 'saturday afternoon'" in {
    val now  = LocalDateTime.of(2014, 1, 1, 14, 30)
    val next = now.`with`(DateTimeEvent(NextWeekdayByName(SATURDAY),AtTime(LocalTime.of(16,0))))

    next shouldBe LocalDateTime.of(2014, 1, 4, 16, 0)
  }

}
