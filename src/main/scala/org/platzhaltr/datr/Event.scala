package org.platzhaltr

import java.time._
import java.time.temporal.{Temporal, TemporalAdjuster, TemporalAdjusters}

import scala.math.signum

sealed trait DateEvent extends TemporalAdjuster with ParseResult {
  def adjustInto(temporal: Temporal): Temporal
}
sealed trait TimeEvent extends TemporalAdjuster with ParseResult {
  def adjustInto(temporal: Temporal): Temporal
}

// Formal Dates

case class OnDate(date: Either[MonthDay, LocalDate]) extends DateEvent {
  override def adjustInto(temporal: Temporal): Temporal = {
    val now = LocalDate.from(temporal)
    date match {
      case Left(monthDay) => monthDay.atYear(now.getYear)
      case Right(localDate) => localDate
    }
  }
}

// Relaxed Dates

case class LastWeekdayByName(weekday: DayOfWeek) extends DateEvent {
  override def adjustInto(temporal: Temporal): Temporal = {
    temporal.`with`(TemporalAdjusters.previous(weekday))
  }
}

case class NextWeekdayByName(weekday: DayOfWeek) extends DateEvent {
  override def adjustInto(temporal: Temporal): Temporal = {
    temporal.`with`(TemporalAdjusters.next(weekday))
  }
}

case class NextWeekWeekdayByName(weekday: DayOfWeek) extends DateEvent{
  override def adjustInto(temporal: Temporal): Temporal = {
    val now = LocalDate.from(temporal)
    if (weekday == DayOfWeek.MONDAY)
      now.`with`(NextWeekdayByName(weekday))
    else
      now.`with`(NextWeekdayByName(DayOfWeek.MONDAY)).`with`(NextWeekdayByName(weekday))
  }
}
case class LastMonthByName(month: Month) extends DateEvent{
  override def adjustInto(temporal: Temporal): Temporal = {
    val now = LocalDate.from(temporal)
    now.minusMonths(Calendar.roll( month.getValue, now.getMonthValue, 12))
  }
}

case class NextMonthByName(month: Month) extends DateEvent {
  override def adjustInto(temporal: Temporal): Temporal = {
    val now = LocalDate.from(temporal)
    now.plusMonths(Calendar.roll(now.getMonthValue, month.getValue, 12))
  }
}

case class WeekdayInMonth(count: Int, weekday: DayOfWeek, month: Month) extends DateEvent{
  override def adjustInto(temporal: Temporal): Temporal = {
    val now = LocalDate.from(temporal)
    val firstOfMonth = now.`with`(NextMonthByName(month)).`with`(TemporalAdjusters.firstDayOfMonth())
    firstOfMonth.`with`(TemporalAdjusters.dayOfWeekInMonth(count, weekday))
  }
}

// Relative Dates

case class InDays(years: Int) extends DateEvent{
  override def adjustInto(temporal: Temporal): Temporal = {
    LocalDate.from(temporal).plusDays(years)
  }
}

case class InWeeks(years: Int) extends DateEvent{
  override def adjustInto(temporal: Temporal): Temporal = {
    LocalDate.from(temporal).plusWeeks(years)
  }
}

case class InMonths(years: Int) extends DateEvent{
  override def adjustInto(temporal: Temporal): Temporal = {
    LocalDate.from(temporal).plusMonths(years)
  }
}

case class InYears(years: Int) extends DateEvent{
  override def adjustInto(temporal: Temporal): Temporal = {
    LocalDate.from(temporal).plusYears(years)
  }
}

// Relative Times

case class InSeconds(seconds: Int) extends TimeEvent {
  override def adjustInto(temporal: Temporal): Temporal =
    LocalDateTime.from(temporal).plusSeconds(seconds)
}
case class InMinutes(minutes: Int) extends TimeEvent {
  override def adjustInto(temporal: Temporal): Temporal =
    LocalDateTime.from(temporal).plusMinutes(minutes)
}
case class InHours(hours: Int) extends TimeEvent {
  override def adjustInto(temporal: Temporal): Temporal =
    LocalDateTime.from(temporal).plusHours(hours)
}

// Formal times

case class AtTime(time: LocalTime) extends TimeEvent {
  override def adjustInto(temporal: Temporal): Temporal = {
    val now = LocalDateTime.from(temporal)
    val date = now.toLocalDate.atTime(time)
    if (Calendar.nowBeforeNext(now, time))
      date
    else
      date.plusDays(1)
  }
}

// Combinations
case class DateTimeEvent(dateAdjuster: TemporalAdjuster, timeEvent: TimeEvent) extends TimeEvent {

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

  override def adjustInto(temporal: Temporal): Temporal = {
    val now = LocalDateTime.from(temporal)
    val newTime = now.`with`(timeEvent)
    val newDate = newTime.toLocalDate.`with`(dateAdjuster)

    val dayAdjustment = dateAdjuster match {
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
