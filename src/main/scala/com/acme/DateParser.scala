package com.acme

import org.joda.time.LocalDate
import org.joda.time.LocalDateTime

import org.parboiled2._

class DateParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Event] = rule {
    FormalTimes | RelaxedDates
  }

  def FormalTimes = rule {
    SpecificTime                        ~> ((t) => AtTime(t)) |
    At ~ Space ~ SpecificTime           ~> ((t) => AtTime(t))
  }

  def RelaxedDates = rule {
    Today                               ~> (()  => InDays(0)) |
    Tomorrow                            ~> (()  => InDays(1)) |
    Yesterday                           ~> (()  => InDays(-1)) |
    Next ~ Space ~ MonthToken           ~> (()  => InMonths(1)) |
    Next ~ Space ~ SpecificWeekday      ~> ((w) => NextWeekdayByName(w)) |
    Next ~ Space ~ SpecificMonth        ~> ((m) => NextMonthByName(m)) |
    Next ~ Space ~ Week                 ~> (()  => InWeeks(1)) |
    Next ~ Space ~ Year                 ~> (()  => InYears(1)) |
    Last ~ Space ~ MonthToken           ~> (()  => InMonths(-1)) |
    Last ~ Space ~ SpecificWeekday      ~> ((w) => LastWeekdayByName(w)) |
    Last ~ Space ~ SpecificMonth        ~> ((m) => LastMonthByName(m)) |
    Last ~ Space ~ Week                 ~> (()  => InWeeks(-1)) |
    Last ~ Space ~ Year                 ~> (()  => InYears(-1)) |
    In ~ Space ~ Number ~ Space ~ Day   ~> ((d) => InDays(d)) |
    In ~ Space ~ Number ~ Space ~ Week  ~> ((w) => InWeeks(w)) |
    In ~ Space ~ Number ~ Space ~ MonthToken ~> ((m) => InMonths(m)) |
    In ~ Space ~ Number ~ Space ~ Year ~> ((y) => InYears(y)) |
    Count ~ Space ~ Day ~ Space ~ Ago   ~> ((c) => InDays(-c)) |
    Count ~ Space ~ Week ~ Space ~ Ago  ~> ((c) => InWeeks(-c)) |
    Count ~ Space ~ MonthToken ~ Space ~ Ago ~> ((c) => InMonths(-c)) |
    Count ~ Space ~ Year ~ Space ~ Ago  ~> ((c) => InYears(-c)) |
    Cardinal ~ Space ~ SpecificWeekday ~ Space ~ In ~ Space ~ SpecificMonth ~> ((c,w,m) => WeekdayInMonth(c, w, m))
  }

  def Cardinal = rule { First | Second | Third | Fourth | Fifth}
  def First    = rule { ignoreCase("first")  ~> (() => 1)}
  def Second   = rule { ignoreCase("second") ~> (() => 2)}
  def Third    = rule { ignoreCase("third")  ~> (() => 3)}
  def Fourth   = rule { ignoreCase("fourth") ~> (() => 4)}
  def Fifth    = rule { ignoreCase("fifth")  ~> (() => 5)}

  def Count = rule { One | Two | Three | Four | Five }
  def One   = rule { ignoreCase("one")   ~> (() => 1)}
  def Two   = rule { ignoreCase("two")   ~> (() => 2)}
  def Three = rule { ignoreCase("three") ~> (() => 3)}
  def Four  = rule { ignoreCase("four")  ~> (() => 4)}
  def Five  = rule { ignoreCase("five")  ~> (() => 5)}

  def Ago  = rule { ignoreCase("ago") }
  def In   = rule { ignoreCase("in") }
  def At   = rule { ignoreCase("at") }

  def Last = rule { ignoreCase("last") }
  def Next = rule { ignoreCase("next") }

  def Today       = rule { ignoreCase("today") }
  def Tomorrow    = rule { ignoreCase("tomorrow") }
  def Yesterday   = rule { ignoreCase("yesterday") }
  def Day         = rule { ignoreCase("day")   ~ optional(ignoreCase("s")) }
  def Week        = rule { ignoreCase("week")  ~ optional(ignoreCase("s")) }
  def MonthToken  = rule { ignoreCase("month") ~ optional(ignoreCase("s")) }
  def Year        = rule { ignoreCase("year")  ~ optional(ignoreCase("s")) }

  def Number = rule { capture(Digits) ~> (_.toInt) }
  def Digits = rule { oneOrMore(CharPredicate.Digit) }
  def Colon  = rule { ignoreCase(":") }

  def SpecificTime  = rule { capture(HoursDigits) ~ Colon ~ capture(MinutesDigits) ~> ((h,m) => new Time(h.toInt, m.toInt)) }
  def HoursDigits   = rule { anyOf("01") ~ optional(CharPredicate.Digit) | ("2" ~ optional(anyOf("01234" ))) | anyOf("3456789") }
  def MinutesDigits = rule { anyOf("012345") ~ optional(CharPredicate.Digit)}

  // Absolute Weekday
  def SpecificWeekday: Rule1[Weekday] = rule {Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday}
  def Monday: Rule1[Weekday]    = rule {capture(ignoreCase("monday")    | ignoreCase("mon") ) ~> ((s: String) => Weekday.Monday )}
  def Tuesday: Rule1[Weekday]   = rule {capture(ignoreCase("tuesday")   | ignoreCase("tue") ) ~> ((s: String) => Weekday.Tuesday )}
  def Wednesday: Rule1[Weekday] = rule {capture(ignoreCase("wednesday") | ignoreCase("wed") ) ~> ((s: String) => Weekday.Wednesday )}
  def Thursday: Rule1[Weekday]  = rule {capture(ignoreCase("thursday")  | ignoreCase("thu") ) ~> ((s: String) => Weekday.Thursday )}
  def Friday: Rule1[Weekday]    = rule {capture(ignoreCase("friday")    | ignoreCase("fri") ) ~> ((s: String) => Weekday.Friday )}
  def Saturday: Rule1[Weekday]  = rule {capture(ignoreCase("saturday")  | ignoreCase("sat") ) ~> ((s: String) => Weekday.Saturday )}
  def Sunday: Rule1[Weekday]    = rule {capture(ignoreCase("sunday")    | ignoreCase("sun") ) ~> ((s: String) => Weekday.Sunday )}

  // Absolute Month
  def SpecificMonth: Rule1[Month] = rule {January | Febuary | March | April | May | June | July | August | September | October | November | December}
  def January: Rule1[Month]   = rule {capture(ignoreCase("january")   | ignoreCase("jan") ) ~> ((s: String) => Month.January )}
  def Febuary: Rule1[Month]   = rule {capture(ignoreCase("february")  | ignoreCase("feb") ) ~> ((s: String) => Month.February )}
  def March: Rule1[Month]     = rule {capture(ignoreCase("march")     | ignoreCase("mar") ) ~> ((s: String) => Month.March )}
  def April: Rule1[Month]     = rule {capture(ignoreCase("april")     | ignoreCase("apr") ) ~> ((s: String) => Month.April )}
  def May: Rule1[Month]       = rule {capture(ignoreCase("may"))                            ~> ((s: String) => Month.May )}
  def June: Rule1[Month]      = rule {capture(ignoreCase("june")      | ignoreCase("jun") ) ~> ((s: String) => Month.June )}
  def July: Rule1[Month]      = rule {capture(ignoreCase("july")      | ignoreCase("jul") ) ~> ((s: String) => Month.July )}
  def August: Rule1[Month]    = rule {capture(ignoreCase("august")    | ignoreCase("aug") ) ~> ((s: String) => Month.August )}
  def September: Rule1[Month] = rule {capture(ignoreCase("september") | ignoreCase("sep") ) ~> ((s: String) => Month.September )}
  def October: Rule1[Month]   = rule {capture(ignoreCase("october")   | ignoreCase("oct") ) ~> ((s: String) => Month.October )}
  def November: Rule1[Month]  = rule {capture(ignoreCase("november")  | ignoreCase("nov") ) ~> ((s: String) => Month.November )}
  def December: Rule1[Month]  = rule {capture(ignoreCase("december")  | ignoreCase("dec") ) ~> ((s: String) => Month.December )}

  def Space = rule { zeroOrMore(" ") }
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

case class Time(hours: Int, minutes: Int) {
  require(hours >= 0 && hours <= 24)
  require(minutes >= 0 && hours <= 59)
}


object Event {
  def roll(now: Int, then: Int, border: Int) = {
    if (now == then) border else (then - now + border) % border
  }
}

sealed trait Event

case class LastWeekdayByName(weekday: Weekday) extends Event {
  def process(now: LocalDate) = now.minusDays( Event.roll(weekday.value, now.getDayOfWeek, 7))
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
