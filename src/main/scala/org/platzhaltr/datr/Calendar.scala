package org.platzhaltr.datr

import java.time.{LocalDateTime, LocalTime}

object Calendar {
  def roll(now: Int, next: Int, border: Int) = {
    if (now == next) border else (next - now + border) % border
  }

  def nowBeforeNext(now: LocalDateTime, next: LocalTime): Boolean = {
    now.getHour < next.getHour || (now.getHour == next.getHour && now.getMinute < next.getMinute)
  }
}

