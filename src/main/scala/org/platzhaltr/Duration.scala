package org.platzhaltr

import org.joda.time.{Duration, Interval, LocalDate, LocalDateTime}

sealed trait DateDuration {
 def process(now: LocalDate): Interval
}
sealed trait TimeDuration {
 def process(now: LocalDateTime): Interval
}

case class ForSeconds(seconds: Int) extends TimeDuration   {
  override def process(now: LocalDateTime): Interval = {
    new Interval(now.toDateTime(), Duration.standardSeconds(seconds))
  }
}

case class ForMinutes(minutes: Int) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    new Interval(now.toDateTime(), Duration.standardMinutes(minutes))
  }
}

case class ForHours(hours: Int) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    new Interval(now.toDateTime(), Duration.standardHours(hours))
  }
}

case class ForDays(days: Int) extends DateDuration {
  override def process(today: LocalDate): Interval = {
    new Interval(today.toDateMidnight(), Duration.standardDays(days))
  }
}

case class FromTimeToTime(from: AtTime, to: AtTime) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    val fromTime = from.process(now)
    new Interval(fromTime.toDateTime(), to.process(fromTime).toDateTime())
  }
}

case class UntilTime(atTime: AtTime) extends TimeDuration {
  override def process(now: LocalDateTime): Interval = {
    new Interval(now.toDateTime(), atTime.process(now).toDateTime())
  }
}

case class UntilWeekday(weekday: Weekday) extends DateDuration {

  override def process(start: LocalDate): Interval = {
    val finishWeekday = NextWeekdayByName(weekday).process(start)

    new Interval(start.toDateMidnight(), finishWeekday.toDateMidnight())
  }
}

case class RelativeDateDuration(dateEvent: DateEvent, dateDuration: DateDuration) extends DateDuration {
  override def process(today: LocalDate): Interval = {
    dateDuration.process(dateEvent.process(today))
  }
}
