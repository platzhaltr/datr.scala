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

  "next week" in { td =>
    parse(td.name) shouldBe InWeeks(1)
  }

  "in 3 weeks" in { td =>
    parse(td.name) shouldBe InWeeks(3)
  }

  "second saturday in september" in { td =>
    parse(td.name) shouldBe WeekdayInMonth(2, Saturday, September)
  }

  def parse(line: String): Datum = {
    val parser = new DateParser(line)
    parser.InputLine.run() match {
      case Success(result) => result
      case Failure(e)      => throw new IllegalArgumentException(e)
    }
  }

}
