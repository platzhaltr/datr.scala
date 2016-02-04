package org.platzhaltr

import java.time.LocalDateTime

object Calendar {
  def roll(now: Int, next: Int, border: Int) = {
    if (now == next) border else (next - now + border) % border
  }

  def nowBeforeNext(now: LocalDateTime, next: Time): Boolean = {
    now.getHour < next.hours || (now.getHour == next.hours && now.getMinute < next.minutes)
  }
}

case class Date(month: Int, day: Int,year: Option[Int] = None) {
  require(month >= 1 && month <= 12)
  // TODO stricter day checking?
  require(day >= 1 && day <= 31)
}

case class Time(hours: Int, minutes: Int) {
  require(hours >= 0 && hours <= 24)
  require(minutes >= 0 && hours <= 59)
}
