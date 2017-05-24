package org.platzhaltr

import java.time.{LocalDateTime, LocalTime}

object Calendar {
  def roll(now: Int, next: Int, border: Int) = {
    if (now == next) border else (next - now + border) % border
  }

  def nowBeforeNext(now: LocalDateTime, next: LocalTime): Boolean = {
    now.getHour < next.getHour || (now.getHour == next.getHour && now.getMinute < next.getMinute)
  }
}

case class Date(month: Int, day: Int,year: Option[Int] = None) {
  require(month >= 1 && month <= 12)
  // TODO stricter day checking?
  require(day >= 1 && day <= 31)
}
