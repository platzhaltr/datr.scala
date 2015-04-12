package org.platzhaltr

import org.scalatest._

import org.joda.time.{Duration,Interval,LocalDate,LocalDateTime}

import Weekday._

class DurationSpec extends FlatSpec with Matchers {

  it should "interpret 'for <n> seconds'" in {
    val now = new LocalDateTime(2014, 1, 1, 18, 30, 30)

    ForSeconds(10).process(now) shouldBe new Interval(now.toDateTime, Duration.standardSeconds(10))
  }

  it should "interpret 'for <n> minutes'" in {
    val now = new LocalDateTime(2014, 1, 1, 18, 30)

    ForMinutes(3).process(now) shouldBe new Interval(now.toDateTime, Duration.standardMinutes(3))
  }

  it should "interpret 'for <n> hours'" in {
    val now = new LocalDateTime(2014, 1, 1, 18, 30)

    ForHours(6).process(now) shouldBe new Interval(now.toDateTime, Duration.standardHours(6))
  }

  it should "interpret 'for <n> days'" in {
    val today = new LocalDate(1970, 6, 20)
    ForDays(3).process(today) shouldBe new Interval(today.toDateTimeAtStartOfDay, Duration.standardDays(3))
  }

  it should "interpret 'for <n> days starting yesterday'" in {
    val today = new LocalDate(1970, 6, 20)

    RelativeDateDuration(InDays(-1),ForDays(3)).process(today) shouldBe new Interval(today.minusDays(1).toDateTimeAtStartOfDay, Duration.standardDays(3))
  }

  it should "interpret 'for <n> days starting today'" in {
    val today = new LocalDate(1970, 6, 20)

    RelativeDateDuration(InDays(0),ForDays(3)).process(today) shouldBe new Interval(today.toDateTimeAtStartOfDay, Duration.standardDays(3))
  }

  it should "interpret 'for <n> days starting tomorrow'" in {
    val today = new LocalDate(1970, 6, 20)

    RelativeDateDuration(InDays(1),ForDays(3)).process(today) shouldBe new Interval(today.plusDays(1).toDateTimeAtStartOfDay, Duration.standardDays(3))
  }

  it should "interpret 'for <n> days starting next monday'" in {
    val today = new LocalDate(1970, 6, 20)
    val nextMonday = new LocalDate(1970, 6, 22)

    RelativeDateDuration(NextWeekdayByName(Monday),ForDays(3)).process(today) shouldBe new Interval(nextMonday.toDateTimeAtStartOfDay, Duration.standardDays(3))
  }

  it should "interpret 'from <time> [to|till|until] <time>'" in {
    val now = new LocalDateTime(2014, 1, 1, 18, 0)
    val from = new LocalDateTime(2014, 1, 1, 18, 30)
    val to = new LocalDateTime(2014, 1, 1, 19, 0)

    FromTimeToTime(AtTime(Time(18,30)), AtTime(Time(19,0))).process(now) shouldBe new Interval(from.toDateTime, to.toDateTime)
  }

  it should "interpret 'till <time>'" in {
    val now = new LocalDateTime(2014, 1, 1, 18, 30)
    val next = new LocalDateTime(2014, 1, 1, 19, 0)

    UntilTime(AtTime(Time(19,0))).process(now) shouldBe new Interval(now.toDateTime, next.toDateTime)
  }

  it should "interpret 'till <time>', time before now" in {
    val now = new LocalDateTime(2014, 1, 1, 18, 30)
    val next = new LocalDateTime(2014, 1, 2, 17, 0)

    UntilTime(AtTime(Time(17,0))).process(now) shouldBe new Interval(now.toDateTime, next.toDateTime)
  }

  it should "interpret 'till <weekday>'" in {
    val today = new LocalDate(1970, 6, 20)
    val nextMonday = new LocalDate(1970, 6, 22)

    val expected = new Interval(today.toDateTimeAtStartOfDay, nextMonday.toDateTimeAtStartOfDay)

    RelativeDateDuration(InDays(0),UntilWeekday(Monday)).process(today) shouldBe expected
  }

  it should "interpret 'from <weekday> to <weekday>'" in {
    val today = new LocalDate(1970, 6, 20)
    val nextMonday = new LocalDate(1970, 6, 22)
    val fridayAfterMonday = new LocalDate(1970, 6, 26)

    val expected = new Interval(nextMonday.toDateTimeAtStartOfDay, fridayAfterMonday.toDateTimeAtStartOfDay)

    RelativeDateDuration(NextWeekdayByName(Monday),UntilWeekday(Friday)).process(today) shouldBe expected
  }

}
