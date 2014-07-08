package com.acme

import org.joda.time.LocalDate
import org.joda.time.LocalDateTime

object Event {
  def roll(now: Int, then: Int, border: Int) = {
    if (now == then) border else (then - now + border) % border
  }
}

sealed trait Event

// Formal Dates

case class OnDate(date: Date) extends Event {
  def process = new LocalDate(date.year, date.month, date.day)
}

// Relaxed Dates

case class LastWeekdayByName(weekday: Weekday) extends Event {
  def process(now: LocalDate) = now.minusDays(Event.roll(weekday.value, now.getDayOfWeek, 7))
}
case class NextWeekdayByName(weekday: Weekday) extends Event {
  def process(now: LocalDate) = now.plusDays(Event.roll(now.getDayOfWeek, weekday.value, 7))
}
case class LastMonthByName(month: Month) extends Event {
  def process(now: LocalDate) = now.minusMonths(Event.roll(month.value, now.getMonthOfYear, 12))
}
case class NextMonthByName(month: Month) extends Event {
  def process(now: LocalDate) = now.plusMonths(Event.roll(now.getMonthOfYear, month.value, 12))
}
case class InDays(days: Int) extends Event {
  def process(now: LocalDate) = now.plusDays(days)
}
case class InWeeks(weeks: Int) extends Event {
  def process(now: LocalDate) = now.plusWeeks(weeks)
}
case class InMonths(months: Int) extends Event {
  def process(now: LocalDate) = now.plusMonths(months)
}
case class InYears(years: Int) extends Event {
  def process(now: LocalDate) = now.plusYears(years)
}
case class WeekdayInMonth(count: Int, weekday: Weekday, month: Month) extends Event {
  def process(now: LocalDate) = {
    val nextMonth = NextMonthByName(month).process(now)
    val firstOfMonth = nextMonth.withDayOfMonth(1)

    val firstOccurence = if (firstOfMonth.getDayOfWeek == weekday.value)
                          firstOfMonth else NextWeekdayByName(weekday).process(firstOfMonth)

    // TODO guard against user error, check if still in correct month
    InWeeks(count - 1).process(firstOccurence)
  }
}

// Formal times

case class AtTime(time: Time) extends Event {
  def process(now: LocalDateTime) = {
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
