package org.platzhaltr


import org.joda.time.LocalDateTime

object Calendar {
  def roll(now: Int, next: Int, border: Int) = {
    if (now == next) border else (next - now + border) % border
  }

  def nowBeforeNext(now: LocalDateTime, next: Time): Boolean = {
    now.getHourOfDay < next.hours || (now.getHourOfDay == next.hours && now.getMinuteOfHour < next.minutes)
  }
}

object Weekday {
  val Monday    = Weekday(1)
  val Tuesday   = Weekday(2)
  val Wednesday = Weekday(3)
  val Thursday  = Weekday(4)
  val Friday    = Weekday(5)
  val Saturday  = Weekday(6)
  val Sunday    = Weekday(7)
}
case class Weekday(val value: Int) {
  require(value >= 1 && value <= 7)
}

object Month {
  val January   = Month(1)
  val February  = Month(2)
  val March     = Month(3)
  val April     = Month(4)
  val May       = Month(5)
  val June      = Month(6)
  val July      = Month(7)
  val August    = Month(8)
  val September = Month(9)
  val October   = Month(10)
  val November  = Month(11)
  val December  = Month(12)
}
case class Month(val value: Int) {
  require(value >= 1 && value <= 12)
}

case class DateTime(date: Date, time: Time)

case class Date(month: Int, day: Int,year: Option[Int] = None) {
  require(month >= 1 && month <= 12)
  // TODO stricter day checking?
  require(day >= 1 && day <= 31)
}

case class Time(hours: Int, minutes: Int) {
  require(hours >= 0 && hours <= 24)
  require(minutes >= 0 && hours <= 59)
}
