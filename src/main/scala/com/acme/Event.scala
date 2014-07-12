package com.acme

import org.joda.time.LocalDate
import org.joda.time.LocalDateTime

object Event {
  def roll(now: Int, next: Int, border: Int) = {
    if (now == next) border else (next - now + border) % border
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

    if (now.getHourOfDay < time.hours || (now.getHourOfDay == time.hours && now.getMinuteOfHour < time.minutes))
      date
    else
      date.plusDays(1)
  }
}

// Combinations

case class DateTimeEvent(dateEvent: DateEvent, timeEvent: TimeEvent) extends TimeEvent {
  override def process(now: LocalDateTime) = {
    val newTime = timeEvent.process(now)
    val newDate = dateEvent.process(newTime.toLocalDate)


    new LocalDateTime(newDate.getYear, newDate.getMonthOfYear, newDate.getDayOfMonth, newTime.getHourOfDay, newTime.getMinuteOfHour)
  }

}
