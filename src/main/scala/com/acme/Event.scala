package com.acme

import org.joda.time.LocalDate
import org.joda.time.LocalDateTime

import scala.math.signum

object Event {
  def roll(now: Int, next: Int, border: Int) = {
    if (now == next) border else (next - now + border) % border
  }

  def nowBeforeNext(now: LocalDateTime, next: Time): Boolean = {
    now.getHourOfDay < next.hours || (now.getHourOfDay == next.hours && now.getMinuteOfHour < next.minutes)
  }
}

sealed trait DateEvent {
  def process(now: LocalDate): LocalDate
}
sealed trait TimeEvent {
  def process(now: LocalDateTime): LocalDateTime
}

// Formal Dates

case class OnDate(date: Date) extends DateEvent {
  override def process(now: LocalDate) = new LocalDate(date.year, date.month, date.day)
}

// Relaxed Dates

case class LastWeekdayByName(weekday: Weekday) extends DateEvent {
  override def process(now: LocalDate) = now.minusDays(Event.roll(weekday.value, now.getDayOfWeek, 7))
}
case class NextWeekdayByName(weekday: Weekday) extends DateEvent {
  override def process(now: LocalDate) = now.plusDays(Event.roll(now.getDayOfWeek, weekday.value, 7))
}
case class LastMonthByName(month: Month) extends DateEvent {
  override def process(now: LocalDate) = now.minusMonths(Event.roll(month.value, now.getMonthOfYear, 12))
}
case class NextMonthByName(month: Month) extends DateEvent {
  override def process(now: LocalDate) = now.plusMonths(Event.roll(now.getMonthOfYear, month.value, 12))
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
    val date = new LocalDateTime(now.getYear, now.getMonthOfYear, now.getDayOfMonth, time.hours, time.minutes)

    val nowHour     = now.getHourOfDay
    val nowMinutes  = now.getMinuteOfHour
    val thenHour    = time.hours
    val thenMinutes = time.minutes

    if (Event.nowBeforeNext(now, time))
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
          if (Event.nowBeforeNext(now,time)) 0 else -1
        } else {
          if (Event.nowBeforeNext(now,time)) 0 else -1
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

    val result = new LocalDateTime(newDate.getYear, newDate.getMonthOfYear, newDate.getDayOfMonth, newTime.getHourOfDay, newTime.getMinuteOfHour)
    result.plusDays(dayAdjustment)
  }

}
