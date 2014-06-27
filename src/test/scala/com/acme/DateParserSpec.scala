package com.acme

import scala.util.{Failure, Success}
import org.scalatest._

import Weekday._
import Month._

class DateParserSpec extends FlatSpec with Matchers {

  it should "parse 'second saturday in september'" in {
    parse("second saturday in september") shouldBe WeekdayInMonth(2, Saturday, September)
  }

  def parse(line: String): Datum = {
    val parser = new DateParser(line)
    parser.InputLine.run() match {
      case Success(result) => result
      case Failure(e)      => throw new IllegalArgumentException(e)
    }
  }

}
