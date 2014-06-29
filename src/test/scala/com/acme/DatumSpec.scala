package com.acme

import org.scalatest._

import org.joda.time.LocalDate

import Weekday._
import Month._

class DatumSpec extends FlatSpec with Matchers {

  it should "interpret last <weekday>" in {
    val today = new LocalDate(2014, 6, 27) // Friday

    LastWeekdayByName(Thursday).toDate(today)  shouldBe new LocalDate(2014, 6, 26)
    LastWeekdayByName(Monday).toDate(today)    shouldBe new LocalDate(2014, 6, 23)
    LastWeekdayByName(Tuesday).toDate(today)   shouldBe new LocalDate(2014, 6, 24)
    LastWeekdayByName(Wednesday).toDate(today) shouldBe new LocalDate(2014, 6, 25)
    LastWeekdayByName(Friday).toDate(today)    shouldBe new LocalDate(2014, 6, 20)
    LastWeekdayByName(Saturday).toDate(today)  shouldBe new LocalDate(2014, 6, 21)
    LastWeekdayByName(Sunday).toDate(today)    shouldBe new LocalDate(2014, 6, 22)
  }

  it should "interpret next <weekday>" in {
    val today = new LocalDate(2014, 6, 27) // Friday

    NextWeekdayByName(Monday).toDate(today)    shouldBe new LocalDate(2014, 6, 30)
    NextWeekdayByName(Tuesday).toDate(today)   shouldBe new LocalDate(2014, 7, 1)
    NextWeekdayByName(Wednesday).toDate(today) shouldBe new LocalDate(2014, 7, 2)
    NextWeekdayByName(Thursday).toDate(today)  shouldBe new LocalDate(2014, 7, 3)
    NextWeekdayByName(Friday).toDate(today)    shouldBe new LocalDate(2014, 7, 4)
    NextWeekdayByName(Saturday).toDate(today)  shouldBe new LocalDate(2014, 6, 28)
    NextWeekdayByName(Sunday).toDate(today)    shouldBe new LocalDate(2014, 6, 29)
  }

  it should "interpret last <month>" in {
    val today = new LocalDate(2014, 6, 1)

    LastMonthByName(January).toDate(today)   shouldBe new LocalDate(2014, 1, 1)
    LastMonthByName(February).toDate(today)  shouldBe new LocalDate(2014, 2, 1)
    LastMonthByName(March).toDate(today)     shouldBe new LocalDate(2014, 3, 1)
    LastMonthByName(April).toDate(today)     shouldBe new LocalDate(2014, 4, 1)
    LastMonthByName(May).toDate(today)       shouldBe new LocalDate(2014, 5, 1)
    LastMonthByName(June).toDate(today)      shouldBe new LocalDate(2013, 6, 1)
    LastMonthByName(July).toDate(today)      shouldBe new LocalDate(2013, 7, 1)
    LastMonthByName(August).toDate(today)    shouldBe new LocalDate(2013, 8, 1)
    LastMonthByName(September).toDate(today) shouldBe new LocalDate(2013, 9, 1)
    LastMonthByName(October).toDate(today)   shouldBe new LocalDate(2013, 10, 1)
    LastMonthByName(November).toDate(today)  shouldBe new LocalDate(2013, 11, 1)
    LastMonthByName(December).toDate(today)  shouldBe new LocalDate(2013, 12, 1)
  }

  it should "interpret next <month>" in {
    val today = new LocalDate(2014, 1, 1)

    NextMonthByName(January).toDate(today)   shouldBe new LocalDate(2015, 1, 1)
    NextMonthByName(February).toDate(today)  shouldBe new LocalDate(2014, 2, 1)
    NextMonthByName(March).toDate(today)     shouldBe new LocalDate(2014, 3, 1)
    NextMonthByName(April).toDate(today)     shouldBe new LocalDate(2014, 4, 1)
    NextMonthByName(May).toDate(today)       shouldBe new LocalDate(2014, 5, 1)
    NextMonthByName(June).toDate(today)      shouldBe new LocalDate(2014, 6, 1)
    NextMonthByName(July).toDate(today)      shouldBe new LocalDate(2014, 7, 1)
    NextMonthByName(August).toDate(today)    shouldBe new LocalDate(2014, 8, 1)
    NextMonthByName(September).toDate(today) shouldBe new LocalDate(2014, 9, 1)
    NextMonthByName(October).toDate(today)   shouldBe new LocalDate(2014, 10, 1)
    NextMonthByName(November).toDate(today)  shouldBe new LocalDate(2014, 11, 1)
    NextMonthByName(December).toDate(today)  shouldBe new LocalDate(2014, 12, 1)
  }

  it should "interpret <count> <weekday> in <month>" in {
    val today = new LocalDate(2014, 1, 1)

    WeekdayInMonth(1, Saturday, July).toDate(today) shouldBe new LocalDate(2014, 7, 5)
    WeekdayInMonth(2, Saturday, July).toDate(today) shouldBe new LocalDate(2014, 7, 12)
  }
}
