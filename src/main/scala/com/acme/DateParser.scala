package com.acme

import org.joda.time.LocalDate
import org.parboiled2._

class DateParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Datum] = rule {
    RelativeFuture
  }

  def RelativeFuture = rule {
    Next ~ Space ~ WeekdayLiteral      ~> ((w) => NextWeekday(w)) |
    Next ~ Space ~ MonthLiteral        ~> ((m) => NextMonth(m)) |
    Next ~ Space ~ Week                ~> (()  => InWeeks(1)) |
    Last ~ Space ~ WeekdayLiteral      ~> ((w) => LastWeekday(w)) |
    Last ~ Space ~ MonthLiteral        ~> ((m) => LastMonth(m)) |
    Last ~ Space ~ Week                ~> (()  => InWeeks(-1)) |
    In ~ Space ~ Number ~ Space ~ Week ~> ((w) => InWeeks(w)) |
    In ~ Space ~ Number ~ Space ~ Day  ~> ((d) => InDays(d)) |
    Count ~ Space ~ WeekdayLiteral ~ Space ~ In ~ Space ~ MonthLiteral ~> ((c,w,m) => WeekdayInMonth(c, w, m))
  }

  def Count  = rule { First | Second | Third | Fourth | Fifth}
  def First  = rule { ignoreCase("first")  ~> (() => 1)}
  def Second = rule { ignoreCase("second") ~> (() => 2)}
  def Third  = rule { ignoreCase("third")  ~> (() => 3)}
  def Fourth = rule { ignoreCase("fourth") ~> (() => 4)}
  def Fifth  = rule { ignoreCase("fifth")  ~> (() => 5)}

  def Last = rule { ignoreCase("last") }
  def Next = rule { ignoreCase("next") }
  def In   = rule { ignoreCase("in") }
  def Day  = rule { ignoreCase("day")  ~ optional(ignoreCase("s")) }
  def Week = rule { ignoreCase("week") ~ optional(ignoreCase("s")) }

  def Number = rule { capture(Digits) ~> (_.toInt) }
  def Digits = rule { oneOrMore(CharPredicate.Digit) }

  // Absolute Weekday
  def WeekdayLiteral: Rule1[Weekday] = rule {Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday}
  def Monday: Rule1[Weekday]    = rule {capture(ignoreCase("monday")    | ignoreCase("mon") ) ~> ((s: String) => Weekday.Monday )}
  def Tuesday: Rule1[Weekday]   = rule {capture(ignoreCase("tuesday")   | ignoreCase("tue") ) ~> ((s: String) => Weekday.Tuesday )}
  def Wednesday: Rule1[Weekday] = rule {capture(ignoreCase("wednesday") | ignoreCase("wed") ) ~> ((s: String) => Weekday.Wednesday )}
  def Thursday: Rule1[Weekday]  = rule {capture(ignoreCase("thursday")  | ignoreCase("thu") ) ~> ((s: String) => Weekday.Thursday )}
  def Friday: Rule1[Weekday]    = rule {capture(ignoreCase("friday")    | ignoreCase("fri") ) ~> ((s: String) => Weekday.Friday )}
  def Saturday: Rule1[Weekday]  = rule {capture(ignoreCase("saturday")  | ignoreCase("sat") ) ~> ((s: String) => Weekday.Saturday )}
  def Sunday: Rule1[Weekday]    = rule {capture(ignoreCase("sunday")    | ignoreCase("sun") ) ~> ((s: String) => Weekday.Sunday )}

  // Absolute Month
  def MonthLiteral: Rule1[Month] = rule {January | Febuary | March | April | May | June | July | August | September | October | November | December}
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

object Datum {
  def roll(now: Int, then: Int, border: Int) = {
    if (now == then) border else (then - now + border) % border
  }
}
sealed trait Datum {
  def toDate(now: LocalDate): LocalDate
}
case class LastWeekday(weekday: Weekday) extends Datum {
  override def toDate(now: LocalDate) = now.minusDays( Datum.roll(weekday.value, now.getDayOfWeek, 7))
}
case class NextWeekday(weekday: Weekday) extends Datum {
  override def toDate(now: LocalDate) = now.plusDays(Datum.roll(now.getDayOfWeek, weekday.value, 7))
}
case class LastMonth(month: Month) extends Datum {
  override def toDate(now: LocalDate) = now.minusMonths(Datum.roll(month.value, now.getMonthOfYear, 12))
}
case class NextMonth(month: Month) extends Datum {
  override def toDate(now: LocalDate) = now.plusMonths(Datum.roll(now.getMonthOfYear, month.value, 12))
}
case class InMonths(months: Int) extends Datum {
  override def toDate(now: LocalDate) = now.plusMonths(months)
}
case class InWeeks(weeks: Int) extends Datum {
  override def toDate(now: LocalDate) = now.plusWeeks(weeks)
}
case class InDays(days: Int) extends Datum {
  override def toDate(now: LocalDate) = now.plusDays(days)
}
case class WeekdayInMonth(count: Int, weekday: Weekday, month: Month) extends Datum {
  override def toDate(now: LocalDate) = {
    val nextMonth = NextMonth(month).toDate(now)
    val firstOfMonth = nextMonth.withDayOfMonth(1)

    val firstOccurence = if (firstOfMonth.getDayOfWeek == weekday.value)
                          firstOfMonth else NextWeekday(weekday).toDate(firstOfMonth)

    // TODO guard against user error, check if still in correct month
    InWeeks(count - 1).toDate(firstOccurence)
  }
}

