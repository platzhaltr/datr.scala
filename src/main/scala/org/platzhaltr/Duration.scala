package org.platzhaltr

import java.time.{Duration, LocalDate, LocalDateTime, ZoneOffset}
import org.threeten.extra.Interval

sealed trait DateDuration extends ParseResult {
 def process(now: LocalDate): Interval
}
sealed trait TimeDuration extends ParseResult {
 def process(now: LocalDateTime): Interval
}

case class ForSeconds(seconds: Int) extends TimeDuration   {
  override def process(now: LocalDateTime): Interval = {
    // TODO which time zone?
    Interval.of(now.toInstant(ZoneOffset.UTC), Duration.ofSeconds(seconds))
  }
}

case class ForMinutes(minutes: Int) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    // TODO which time zone?
    Interval.of(now.toInstant(ZoneOffset.UTC), Duration.ofMinutes(minutes))
  }
}

case class ForHours(hours: Int) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    // TODO which time zone?
    Interval.of(now.toInstant(ZoneOffset.UTC), Duration.ofHours(hours))
  }
}

case class ForDays(days: Int) extends DateDuration {
  override def process(today: LocalDate): Interval = {
    // TODO which time zone?
    Interval.of(today.atStartOfDay().toInstant(ZoneOffset.UTC), Duration.ofDays(days))
  }
}

case class FromTimeToTime(from: AtTime, to: AtTime) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    val fromTime = from.process(now)
    // TODO which time zone?
    Interval.of(fromTime.toInstant(ZoneOffset.UTC), to.process(fromTime).toInstant(ZoneOffset.UTC))
  }
}

case class FromDateTimeToDateTime(from: DateTimeEvent, to: DateTimeEvent) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    // TODO which time zone?
    // TODO rename vals
    val fromDateTime = from.process(now)
    val nextDateTime = to.process(fromDateTime).toInstant(ZoneOffset.UTC)

    Interval.of(fromDateTime.toInstant(ZoneOffset.UTC), nextDateTime)
  }
}

case class UntilTime(atTime: AtTime) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    // TODO which time zone?
    Interval.of(now.toInstant(ZoneOffset.UTC), atTime.process(now).toInstant(ZoneOffset.UTC))
  }
}

case class UntilWeekday(weekday: Weekday) extends DateDuration {

  override def process(start: LocalDate): Interval = {
    val finishWeekday = NextWeekdayByName(weekday).process(start)

    // TODO which time zone?
    Interval.of(start.atStartOfDay().toInstant(ZoneOffset.UTC), finishWeekday.atStartOfDay().toInstant(ZoneOffset.UTC))
  }
}

case class RelativeDateDuration(dateEvent: DateEvent, dateDuration: DateDuration) extends DateDuration {
  override def process(today: LocalDate): Interval = {
    dateDuration.process(dateEvent.process(today))
  }
}
