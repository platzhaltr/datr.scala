package org.platzhaltr

import java.time.{Duration, LocalDate, LocalDateTime}
import org.threeten.extra.Interval
import scala.math.signum

sealed trait DateEvent extends ParseResult {
  def process(now: LocalDate): LocalDate
}
sealed trait TimeEvent extends ParseResult {
  def process(now: LocalDateTime): LocalDateTime
}

// Formal Dates

case class OnDate(date: Date) extends DateEvent {
  override def process(now: LocalDate) =

  LocalDate.of(date.year.getOrElse(now.getYear), date.month, date.day)
}

// Relaxed Dates

case class LastWeekdayByName(weekday: Weekday) extends DateEvent {
  override def process(now: LocalDate) = now.minusDays(Calendar.roll(weekday.value, now.getDayOfWeek.getValue, 7))
}
case class NextWeekdayByName(weekday: Weekday) extends DateEvent {
  override def process(now: LocalDate) = now.plusDays(Calendar.roll(now.getDayOfWeek.getValue, weekday.value, 7))
}
case class LastMonthByName(month: Month) extends DateEvent {
  override def process(now: LocalDate) = now.minusMonths(Calendar.roll(month.value, now.getMonthValue, 12))
}
case class NextMonthByName(month: Month) extends DateEvent {
  override def process(now: LocalDate) = now.plusMonths(Calendar.roll(now.getMonthValue, month.value, 12))
}
case class WeekdayInMonth(count: Int, weekday: Weekday, month: Month) extends DateEvent {
  override def process(now: LocalDate) = {
    val nextMonth = NextMonthByName(month).process(now)
    val firstOfMonth = nextMonth.withDayOfMonth(1)

    val firstOccurence = if (firstOfMonth.getDayOfWeek == weekday.value)
                          firstOfMonth else NextWeekdayByName(weekday).process(firstOfMonth)

    // TODO guard against user error, check if still in correct month
    InWeeks(count - 1).process(firstOccurence)
  }
}

// Relative Dates

case class InDays(days: Int) extends DateEvent {
  override def process(now: LocalDate) = now.plusDays(days)
}
case class InWeeks(weeks: Int) extends DateEvent {
  override def process(now: LocalDate) = now.plusWeeks(weeks)
}
case class InMonths(months: Int) extends DateEvent {
  override def process(now: LocalDate) = now.plusMonths(months)
}
case class InYears(years: Int) extends DateEvent {
  override def process(now: LocalDate) = now.plusYears(years)
}

// Relative Times

case class InSeconds(seconds: Int) extends TimeEvent {
  override def process(now: LocalDateTime) = now.plusSeconds(seconds)
}
case class InMinutes(minutes: Int) extends TimeEvent {
  override def process(now: LocalDateTime) = now.plusMinutes(minutes)
}
case class InHours(hours: Int) extends TimeEvent {
  override def process(now: LocalDateTime) = now.plusHours(hours)
}

// Formal times

case class AtTime(time: Time) extends TimeEvent {
  override def process(now: LocalDateTime) = {
    val date = LocalDateTime.of(now.getYear, now.getMonth, now.getDayOfMonth, time.hours, time.minutes)

    val nowHour     = now.getHour
    val nowMinutes  = now.getMinute
    val thenHour    = time.hours
    val thenMinutes = time.minutes

    if (Calendar.nowBeforeNext(now, time))
      date
    else
      date.plusDays(1)
  }
}

// Combinations
case class DateTimeEvent(dateEvent: DateEvent, timeEvent: TimeEvent) extends TimeEvent {

  private def adjustDays(now: LocalDateTime, signum: Int, timeEvent: TimeEvent) = {
    timeEvent match {
      case AtTime(time) =>
        if (signum < 0) {
          0
        } else if (signum == 0) {
          if (Calendar.nowBeforeNext(now,time)) 0 else -1
        } else {
          if (Calendar.nowBeforeNext(now,time)) 0 else -1
        }
      case _ => 0
    }
  }

  override def process(now: LocalDateTime) = {
    val newTime = timeEvent.process(now)
    val newDate = dateEvent.process(newTime.toLocalDate)

    val dayAdjustment = dateEvent match {
      case InDays(days)     => adjustDays(now, signum(days), timeEvent)
      case InWeeks(weeks)   => adjustDays(now, signum(weeks), timeEvent)
      case InMonths(months) => adjustDays(now, signum(months), timeEvent)
      case InYears(years)   => adjustDays(now, signum(years), timeEvent)
      case _ => 0
    }

    val result = LocalDateTime.of(newDate.getYear, newDate.getMonthValue, newDate.getDayOfMonth, newTime.getHour, newTime.getMinute)
    result.plusDays(dayAdjustment)
  }

}
