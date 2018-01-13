package org.platzhaltr.parsing.datr

import org.scalatest._

import java.time._
import java.time.DayOfWeek._
import org.threeten.extra.Interval

class DurationSpec extends FlatSpec with Matchers {

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

  it should "interpret 'from <time> [to|till|until] <time> next <weekday>'" in {
    val now = LocalDateTime.of(2014, 1, 1, 14, 0)
    val from = LocalDateTime.of(2014, 1, 6, 14, 0)
    val to = LocalDateTime.of(2014, 1, 6, 15, 0)

    FromTimeToTime(AtTime(LocalTime.of(14,0)), AtTime(LocalTime.of(15,0)), Some(NextWeekdayByName(MONDAY))).process(now) shouldBe Interval.of(from.toInstant(ZoneOffset.UTC), to.toInstant(ZoneOffset.UTC))
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

}
