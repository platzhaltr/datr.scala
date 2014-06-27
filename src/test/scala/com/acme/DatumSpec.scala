package com.acme

import org.scalatest._

import org.joda.time.LocalDate

import Weekday._
import Month._

class DatumSpec extends FlatSpec with Matchers {

  it should "interpret last <weekday>" in {
    val today = new LocalDate(2014, 6, 27) // Friday

    LastWeekday(Thursday).toDate(today)  shouldBe new LocalDate(2014, 6, 26)
    LastWeekday(Monday).toDate(today)    shouldBe new LocalDate(2014, 6, 23)
    LastWeekday(Tuesday).toDate(today)   shouldBe new LocalDate(2014, 6, 24)
    LastWeekday(Wednesday).toDate(today) shouldBe new LocalDate(2014, 6, 25)

    LastWeekday(Friday).toDate(today)    shouldBe new LocalDate(2014, 6, 20)
    LastWeekday(Saturday).toDate(today)  shouldBe new LocalDate(2014, 6, 21)
    LastWeekday(Sunday).toDate(today)    shouldBe new LocalDate(2014, 6, 22)
  }

  it should "interpret next <weekday>" in {
    val today = new LocalDate(2014, 6, 27) // Friday

    NextWeekday(Monday).toDate(today)    shouldBe new LocalDate(2014, 6, 30)
    NextWeekday(Tuesday).toDate(today)   shouldBe new LocalDate(2014, 7, 1)
    NextWeekday(Wednesday).toDate(today) shouldBe new LocalDate(2014, 7, 2)
    NextWeekday(Thursday).toDate(today)  shouldBe new LocalDate(2014, 7, 3)
    NextWeekday(Friday).toDate(today)    shouldBe new LocalDate(2014, 7, 4)
    NextWeekday(Saturday).toDate(today)  shouldBe new LocalDate(2014, 6, 28)
    NextWeekday(Sunday).toDate(today)    shouldBe new LocalDate(2014, 6, 29)
  }

  it should "interpret last <month>" in {
    val today = new LocalDate(2014, 6, 1)

    LastMonth(January).toDate(today)   shouldBe new LocalDate(2014, 1, 1)
    LastMonth(February).toDate(today)  shouldBe new LocalDate(2014, 2, 1)
    LastMonth(March).toDate(today)     shouldBe new LocalDate(2014, 3, 1)
    LastMonth(April).toDate(today)     shouldBe new LocalDate(2014, 4, 1)
    LastMonth(May).toDate(today)       shouldBe new LocalDate(2014, 5, 1)
    LastMonth(June).toDate(today)      shouldBe new LocalDate(2013, 6, 1)
    LastMonth(July).toDate(today)      shouldBe new LocalDate(2013, 7, 1)
    LastMonth(August).toDate(today)    shouldBe new LocalDate(2013, 8, 1)
    LastMonth(September).toDate(today) shouldBe new LocalDate(2013, 9, 1)
    LastMonth(October).toDate(today)   shouldBe new LocalDate(2013, 10, 1)
    LastMonth(November).toDate(today)  shouldBe new LocalDate(2013, 11, 1)
    LastMonth(December).toDate(today)  shouldBe new LocalDate(2013, 12, 1)
  }

  it should "interpret next <month>" in {
    val today = new LocalDate(2014, 1, 1)

    NextMonth(January).toDate(today)   shouldBe new LocalDate(2015, 1, 1)
    NextMonth(February).toDate(today)  shouldBe new LocalDate(2014, 2, 1)
    NextMonth(March).toDate(today)     shouldBe new LocalDate(2014, 3, 1)
    NextMonth(April).toDate(today)     shouldBe new LocalDate(2014, 4, 1)
    NextMonth(May).toDate(today)       shouldBe new LocalDate(2014, 5, 1)
    NextMonth(June).toDate(today)      shouldBe new LocalDate(2014, 6, 1)
    NextMonth(July).toDate(today)      shouldBe new LocalDate(2014, 7, 1)
    NextMonth(August).toDate(today)    shouldBe new LocalDate(2014, 8, 1)
    NextMonth(September).toDate(today) shouldBe new LocalDate(2014, 9, 1)
    NextMonth(October).toDate(today)   shouldBe new LocalDate(2014, 10, 1)
    NextMonth(November).toDate(today)  shouldBe new LocalDate(2014, 11, 1)
    NextMonth(December).toDate(today)  shouldBe new LocalDate(2014, 12, 1)
  }

  it should "interpret <count> <weekday> in <month>" in {
    val today = new LocalDate(2014, 1, 1)

    WeekdayInMonth(1, Saturday, July).toDate(today) shouldBe new LocalDate(2014, 7, 5)
    WeekdayInMonth(2, Saturday, July).toDate(today) shouldBe new LocalDate(2014, 7, 12)
  }
}
