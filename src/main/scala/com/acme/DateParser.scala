package com.acme

import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scala.io.StdIn
import org.parboiled2._

import org.joda.time.LocalDate

object DateParser extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    // TODO: Replace next three lines with `scala.Predef.readLine(text: String, args: Any*)`
    // once BUG https://issues.scala-lang.org/browse/SI-8167 is fixed
    print("---\nEnter expression > ")
    Console.out.flush()

    StdIn.readLine() match {
      case "" =>
      case line =>
        val parser = new DateParser(line)
        parser.InputLine.run() match {
          case Success(result)        => println("Result: " + result.toDate)
          case Failure(e: ParseError) => println("Invalid expression: " + parser.formatError(e))
          case Failure(e)             => println("Unexpected error: " + e)
        }
        repl()
    }
  }
}

/**
 * This parser reads simple calculator expressions and evaluates them right during
 * the parsing run with the help of the value stack.
 */
class DateParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Config] = rule {
    RelativeFuture
  }

  def RelativeFuture = rule {
    Next ~ Space ~ Weekday ~> ((w) => Config(Seq(w))) |
    Next ~ Space ~ Month   ~> ((m) => Config(Seq(m))) |
    Next ~ Space ~ Week    ~> (() => Config(Seq(new Weeks(1))))
  }

  def Next = rule { ignoreCase("next") }

  def Week = rule { ignoreCase("week") ~ optional(ignoreCase("s")) }

  // Absolute Weekday
  def Weekday: Rule1[Weekday]   = rule {Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday}
  def Monday: Rule1[Weekday]    = rule {capture(ignoreCase("monday")    | ignoreCase("mon") ) ~> ((s: String) => new Weekday(1) )}
  def Tuesday: Rule1[Weekday]   = rule {capture(ignoreCase("tuesday")   | ignoreCase("tue") ) ~> ((s: String) => new Weekday(2) )}
  def Wednesday: Rule1[Weekday] = rule {capture(ignoreCase("wednesday") | ignoreCase("wed") ) ~> ((s: String) => new Weekday(3) )}
  def Thursday: Rule1[Weekday]  = rule {capture(ignoreCase("thursday")  | ignoreCase("thu") ) ~> ((s: String) => new Weekday(4) )}
  def Friday: Rule1[Weekday]    = rule {capture(ignoreCase("friday")    | ignoreCase("fri") ) ~> ((s: String) => new Weekday(5) )}
  def Saturday: Rule1[Weekday]  = rule {capture(ignoreCase("saturday")  | ignoreCase("sat") ) ~> ((s: String) => new Weekday(6) )}
  def Sunday: Rule1[Weekday]    = rule {capture(ignoreCase("sunday")    | ignoreCase("sun") ) ~> ((s: String) => new Weekday(7) )}

  // Absolute Month
  def Month: Rule1[Month]     = rule {January | Febuary | March | April | May | June | July | August | September | October | November | December}
  def January: Rule1[Month]   = rule {capture(ignoreCase("january")   | ignoreCase("jan") ) ~> ((s: String) => new Month(1) )}
  def Febuary: Rule1[Month]   = rule {capture(ignoreCase("february")  | ignoreCase("feb") ) ~> ((s: String) => new Month(2) )}
  def March: Rule1[Month]     = rule {capture(ignoreCase("march")     | ignoreCase("mar") ) ~> ((s: String) => new Month(3) )}
  def April: Rule1[Month]     = rule {capture(ignoreCase("april")     | ignoreCase("apr") ) ~> ((s: String) => new Month(4) )}
  def May: Rule1[Month]       = rule {capture(ignoreCase("may"))                            ~> ((s: String) => new Month(5) )}
  def June: Rule1[Month]      = rule {capture(ignoreCase("june")      | ignoreCase("jun") ) ~> ((s: String) => new Month(6) )}
  def July: Rule1[Month]      = rule {capture(ignoreCase("july")      | ignoreCase("jul") ) ~> ((s: String) => new Month(7) )}
  def August: Rule1[Month]    = rule {capture(ignoreCase("august")    | ignoreCase("aug") ) ~> ((s: String) => new Month(8) )}
  def September: Rule1[Month] = rule {capture(ignoreCase("september") | ignoreCase("sep") ) ~> ((s: String) => new Month(9) )}
  def October: Rule1[Month]   = rule {capture(ignoreCase("october")   | ignoreCase("oct") ) ~> ((s: String) => new Month(10) )}
  def November: Rule1[Month]  = rule {capture(ignoreCase("november")  | ignoreCase("nov") ) ~> ((s: String) => new Month(11) )}
  def December: Rule1[Month]  = rule {capture(ignoreCase("december")  | ignoreCase("dec") ) ~> ((s: String) => new Month(12) )}


  def Space = rule { zeroOrMore(" ") }
}

sealed trait Datum
case class Weekday(d: Int) extends Datum {
  require(d >= 1 && d <= 7)

  val value = d
}
case class Month(m: Int) extends Datum {
  require(m >= 1 && m <= 12)

  val value = m
}
case class Weeks(w: Int) extends Datum {
  val value = w
}

object Config {
  val DaysInWeek = 7
  val MonthsInYear = 12

  def inWeeks(now: LocalDate, weeks: Weeks): LocalDate = {
    now.plusWeeks(weeks.value)
  }

  def nextWeekday(now: LocalDate, weekday: Weekday): LocalDate = {
    now.plusDays(roll(now.getDayOfWeek, weekday.value, DaysInWeek))
  }

  def nextMonth(now: LocalDate, month: Month): LocalDate = {
    now.plusMonths(roll(now.getMonthOfYear, month.value, MonthsInYear))
  }

  private def roll(now: Int, then: Int, border: Int) = {
    if (now == then) border else (then - now + border) % border
  }
}

case class Config(datums: Seq[Datum] = Nil) {

  import Config._

  val toDate: String = {
    val today = new LocalDate()

    val result = datums.head match {
      case month @ Month(_) => nextMonth(today, month).toString
      case weekday @ Weekday(_) => nextWeekday(today, weekday).toString
      case weeks @ Weeks(_) => inWeeks(today, weeks).toString
    }

    s"${result}"
  }
}
