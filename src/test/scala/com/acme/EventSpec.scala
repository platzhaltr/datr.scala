package com.acme

import org.scalatest._

import org.joda.time.{LocalDate,LocalDateTime}

import Weekday._
import Month._

class EventSpec extends FlatSpec with Matchers {

  it should "interpret last <weekday>" in {
    val today = new LocalDate(2014, 6, 27) // Friday

    LastWeekdayByName(Thursday).process(today)  shouldBe new LocalDate(2014, 6, 26)
    LastWeekdayByName(Monday).process(today)    shouldBe new LocalDate(2014, 6, 23)
    LastWeekdayByName(Tuesday).process(today)   shouldBe new LocalDate(2014, 6, 24)
    LastWeekdayByName(Wednesday).process(today) shouldBe new LocalDate(2014, 6, 25)
    LastWeekdayByName(Friday).process(today)    shouldBe new LocalDate(2014, 6, 20)
    LastWeekdayByName(Saturday).process(today)  shouldBe new LocalDate(2014, 6, 21)
    LastWeekdayByName(Sunday).process(today)    shouldBe new LocalDate(2014, 6, 22)
  }

  it should "interpret next <weekday>" in {
    val today = new LocalDate(2014, 6, 27) // Friday

    NextWeekdayByName(Monday).process(today)    shouldBe new LocalDate(2014, 6, 30)
    NextWeekdayByName(Tuesday).process(today)   shouldBe new LocalDate(2014, 7, 1)
    NextWeekdayByName(Wednesday).process(today) shouldBe new LocalDate(2014, 7, 2)
    NextWeekdayByName(Thursday).process(today)  shouldBe new LocalDate(2014, 7, 3)
    NextWeekdayByName(Friday).process(today)    shouldBe new LocalDate(2014, 7, 4)
    NextWeekdayByName(Saturday).process(today)  shouldBe new LocalDate(2014, 6, 28)
    NextWeekdayByName(Sunday).process(today)    shouldBe new LocalDate(2014, 6, 29)
  }

  it should "interpret last <month>" in {
    val today = new LocalDate(2014, 6, 1)

    LastMonthByName(January).process(today)   shouldBe new LocalDate(2014, 1, 1)
    LastMonthByName(February).process(today)  shouldBe new LocalDate(2014, 2, 1)
    LastMonthByName(March).process(today)     shouldBe new LocalDate(2014, 3, 1)
    LastMonthByName(April).process(today)     shouldBe new LocalDate(2014, 4, 1)
    LastMonthByName(May).process(today)       shouldBe new LocalDate(2014, 5, 1)
    LastMonthByName(June).process(today)      shouldBe new LocalDate(2013, 6, 1)
    LastMonthByName(July).process(today)      shouldBe new LocalDate(2013, 7, 1)
    LastMonthByName(August).process(today)    shouldBe new LocalDate(2013, 8, 1)
    LastMonthByName(September).process(today) shouldBe new LocalDate(2013, 9, 1)
    LastMonthByName(October).process(today)   shouldBe new LocalDate(2013, 10, 1)
    LastMonthByName(November).process(today)  shouldBe new LocalDate(2013, 11, 1)
    LastMonthByName(December).process(today)  shouldBe new LocalDate(2013, 12, 1)
  }

  it should "interpret next <month>" in {
    val today = new LocalDate(2014, 1, 1)

    NextMonthByName(January).process(today)   shouldBe new LocalDate(2015, 1, 1)
    NextMonthByName(February).process(today)  shouldBe new LocalDate(2014, 2, 1)
    NextMonthByName(March).process(today)     shouldBe new LocalDate(2014, 3, 1)
    NextMonthByName(April).process(today)     shouldBe new LocalDate(2014, 4, 1)
    NextMonthByName(May).process(today)       shouldBe new LocalDate(2014, 5, 1)
    NextMonthByName(June).process(today)      shouldBe new LocalDate(2014, 6, 1)
    NextMonthByName(July).process(today)      shouldBe new LocalDate(2014, 7, 1)
    NextMonthByName(August).process(today)    shouldBe new LocalDate(2014, 8, 1)
    NextMonthByName(September).process(today) shouldBe new LocalDate(2014, 9, 1)
    NextMonthByName(October).process(today)   shouldBe new LocalDate(2014, 10, 1)
    NextMonthByName(November).process(today)  shouldBe new LocalDate(2014, 11, 1)
    NextMonthByName(December).process(today)  shouldBe new LocalDate(2014, 12, 1)
  }

  it should "interpret <count> <weekday> in <month>" in {
    val today = new LocalDate(2014, 1, 1)

    WeekdayInMonth(1, Saturday, July).process(today) shouldBe new LocalDate(2014, 7, 5)
    WeekdayInMonth(2, Saturday, July).process(today) shouldBe new LocalDate(2014, 7, 12)
  }

  it should "interpret <hour>:<minute> if time in future" in {
    val now = new LocalDateTime(2014, 1, 1, 14, 30)

    AtTime(Time(15,0)).process(now) shouldBe new LocalDateTime(2014, 1, 1, 15, 0)
  }

  it should "interpret <hour>:<minute> if time in past" in {
    val now = new LocalDateTime(2014, 1, 1, 14, 30)

    AtTime(Time(14,0)).process(now) shouldBe new LocalDateTime(2014, 1, 2, 14, 0)
  }
}
