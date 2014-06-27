package com.acme

import org.scalatest._

import org.joda.time.LocalDate

class DateParserSpec extends FlatSpec with Matchers {

  it should "interpret next weekday" in {
    val today =  new LocalDate(2014, 6, 27) // Friday

    val nextMonday = Weekday(1)
    val nextTuesday = Weekday(2)
    val nextWednesday = Weekday(3)
    val nextThursday = Weekday(4)
    val nextFriday = Weekday(5)
    val nextSaturday = Weekday(6)
    val nextSunday = Weekday(7)


    Config.nextWeekday(today, nextMonday) shouldBe new LocalDate(2014, 6, 30)
    Config.nextWeekday(today, nextTuesday) shouldBe new LocalDate(2014, 7, 1)
    Config.nextWeekday(today, nextWednesday) shouldBe new LocalDate(2014, 7, 2)
    Config.nextWeekday(today, nextThursday) shouldBe new LocalDate(2014, 7, 3)
    Config.nextWeekday(today, nextFriday) shouldBe new LocalDate(2014, 7, 4)
    Config.nextWeekday(today, nextSaturday) shouldBe new LocalDate(2014, 6, 28)
    Config.nextWeekday(today, nextSunday) shouldBe new LocalDate(2014, 6, 29)
  }

    it should "interpret next month" in {
    val today =  new LocalDate(2014, 1, 1)

    val nextJanuary = Month(1)
    val nextFebruary = Month(2)
    val nextMarch = Month(3)
    val nextApril = Month(4)
    val nextMay = Month(5)
    val nextJune = Month(6)
    val nextJuly = Month(7)
    val nextAugust = Month(8)
    val nextSeptember = Month(9)
    val nextOctober = Month(10)
    val nextNovember = Month(11)
    val nextDecember = Month(12)

    Config.nextMonth(today, nextJanuary) shouldBe new LocalDate(2015, 1, 1)
    Config.nextMonth(today, nextFebruary) shouldBe new LocalDate(2014, 2, 1)
    Config.nextMonth(today, nextMarch) shouldBe new LocalDate(2014, 3, 1)
    Config.nextMonth(today, nextApril) shouldBe new LocalDate(2014, 4, 1)
    Config.nextMonth(today, nextMay) shouldBe new LocalDate(2014, 5, 1)
    Config.nextMonth(today, nextJune) shouldBe new LocalDate(2014, 6, 1)
    Config.nextMonth(today, nextJuly) shouldBe new LocalDate(2014, 7, 1)
    Config.nextMonth(today, nextAugust) shouldBe new LocalDate(2014, 8, 1)
    Config.nextMonth(today, nextSeptember) shouldBe new LocalDate(2014, 9, 1)
    Config.nextMonth(today, nextOctober) shouldBe new LocalDate(2014, 10, 1)
    Config.nextMonth(today, nextNovember) shouldBe new LocalDate(2014, 11, 1)
    Config.nextMonth(today, nextDecember) shouldBe new LocalDate(2014, 12, 1)
  }
}
