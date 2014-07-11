package com.acme

import scala.util.{Failure, Success}
import org.scalatest._
import org.scalatest.Matchers._

import Weekday._
import Month._

class DateParserSpec extends fixture.FreeSpec with Matchers {

  type FixtureParam = TestData

  override def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(test))
  }

  // Formal dates

  "2014-10-23" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  "20141023" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  "2014/10/23" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  "23/10/2014" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  "on 2014-10-23" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  "on 20141023" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  "on 2014/10/23" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  "on 23/10/2014" in { td =>
    parse(td.name) shouldBe OnDate(Date(2014,10,23))
  }

  // Relaxed dates

  "today" in { td =>
    parse(td.name) shouldBe InDays(0)
  }

  "tomorrow" in { td =>
    parse(td.name) shouldBe InDays(1)
  }

  "yesterday" in { td =>
    parse(td.name) shouldBe InDays(-1)
  }

  "next monday" in { td =>
    parse(td.name) shouldBe NextWeekdayByName(Monday)
  }

  "last monday" in { td =>
    parse(td.name) shouldBe LastWeekdayByName(Monday)
  }

  "last tuesday" in { td =>
    parse(td.name) shouldBe LastWeekdayByName(Tuesday)
  }

  "next tuesday" in { td =>
    parse(td.name) shouldBe NextWeekdayByName(Tuesday)
  }

  "last january" in { td =>
    parse(td.name) shouldBe LastMonthByName(January)
  }

  "next february" in { td =>
    parse(td.name) shouldBe NextMonthByName(February)
  }

  "last week" in { td =>
    parse(td.name) shouldBe InWeeks(-1)
  }

  "next week" in { td =>
    parse(td.name) shouldBe InWeeks(1)
  }

  "last month" in { td =>
    parse(td.name) shouldBe InMonths(-1)
  }

  "next month" in { td =>
    parse(td.name) shouldBe InMonths(1)
  }

  "last year" in { td =>
    parse(td.name) shouldBe InYears(-1)
  }

  "next year" in { td =>
    parse(td.name) shouldBe InYears(1)
  }

  "second saturday in september" in { td =>
    parse(td.name) shouldBe WeekdayInMonth(2, Saturday, September)
  }

  "in 1 day" in { td =>
    parse(td.name) shouldBe InDays(1)
  }

  "in 2 weeks" in { td =>
    parse(td.name) shouldBe InWeeks(2)
  }

  "in 3 months" in { td =>
    parse(td.name) shouldBe InMonths(3)
  }

  "in 4 years" in { td =>
    parse(td.name) shouldBe InYears(4)
  }

  "one day ago" in { td =>
    parse(td.name) shouldBe InDays(-1)
  }

  "two weeks ago" in { td =>
    parse(td.name) shouldBe InWeeks(-2)
  }

  "three months ago" in { td =>
    parse(td.name) shouldBe InMonths(-3)
  }

  "four years ago" in { td =>
    parse(td.name) shouldBe InYears(-4)
  }

  // Formal times

  "5:00" in { td =>
    parse(td.name) shouldBe AtTime(Time(5,0))
  }

  "at 2:00" in { td =>
    parse(td.name) shouldBe AtTime(Time(2,0))
  }

  "6pm" in { td =>
    parse(td.name) shouldBe AtTime(Time(18,0))
  }

  "5:30 a.m." in { td =>
    parse(td.name) shouldBe AtTime(Time(5,30))
  }

  "12:15 am" in { td =>
    parse(td.name) shouldBe AtTime(Time(0,15))
  }

  "12:15 pm" in { td =>
    parse(td.name) shouldBe AtTime(Time(12,15))
  }

  "at 7 pm" in { td =>
    parse(td.name) shouldBe AtTime(Time(19,0))
  }

  def parse(line: String): Event = {
    val parser = new DateParser(line)
    parser.InputLine.run() match {
      case Success(result) => result
      case Failure(e)      => throw new IllegalArgumentException(e)
    }
  }

}
