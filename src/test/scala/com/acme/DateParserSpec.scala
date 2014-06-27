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


    DateParser.nextWeekday(today, nextMonday) shouldBe new LocalDate(2014, 6, 30)
    DateParser.nextWeekday(today, nextTuesday) shouldBe new LocalDate(2014, 7, 1)
    DateParser.nextWeekday(today, nextWednesday) shouldBe new LocalDate(2014, 7, 2)
    DateParser.nextWeekday(today, nextThursday) shouldBe new LocalDate(2014, 7, 3)
    DateParser.nextWeekday(today, nextFriday) shouldBe new LocalDate(2014, 7, 4)
    DateParser.nextWeekday(today, nextSaturday) shouldBe new LocalDate(2014, 6, 28)
    DateParser.nextWeekday(today, nextSunday) shouldBe new LocalDate(2014, 6, 29)
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

    DateParser.nextMonth(today, nextJanuary) shouldBe new LocalDate(2015, 1, 1)
    DateParser.nextMonth(today, nextFebruary) shouldBe new LocalDate(2014, 2, 1)
    DateParser.nextMonth(today, nextMarch) shouldBe new LocalDate(2014, 3, 1)
    DateParser.nextMonth(today, nextApril) shouldBe new LocalDate(2014, 4, 1)
    DateParser.nextMonth(today, nextMay) shouldBe new LocalDate(2014, 5, 1)
    DateParser.nextMonth(today, nextJune) shouldBe new LocalDate(2014, 6, 1)
    DateParser.nextMonth(today, nextJuly) shouldBe new LocalDate(2014, 7, 1)
    DateParser.nextMonth(today, nextAugust) shouldBe new LocalDate(2014, 8, 1)
    DateParser.nextMonth(today, nextSeptember) shouldBe new LocalDate(2014, 9, 1)
    DateParser.nextMonth(today, nextOctober) shouldBe new LocalDate(2014, 10, 1)
    DateParser.nextMonth(today, nextNovember) shouldBe new LocalDate(2014, 11, 1)
    DateParser.nextMonth(today, nextDecember) shouldBe new LocalDate(2014, 12, 1)
  }
}
