package com.acme

import org.scalatest._

import org.joda.time.LocalDate

class DatumSpec extends FlatSpec with Matchers {

  it should "interpret next weekday" in {

    import Weekday._

    val today =  new LocalDate(2014, 6, 27) // Friday

    NextWeekday(Monday).toDate(today)    shouldBe new LocalDate(2014, 6, 30)
    NextWeekday(Tuesday).toDate(today)   shouldBe new LocalDate(2014, 7, 1)
    NextWeekday(Wednesday).toDate(today) shouldBe new LocalDate(2014, 7, 2)
    NextWeekday(Thursday).toDate(today)  shouldBe new LocalDate(2014, 7, 3)
    NextWeekday(Friday).toDate(today)    shouldBe new LocalDate(2014, 7, 4)
    NextWeekday(Saturday).toDate(today)  shouldBe new LocalDate(2014, 6, 28)
    NextWeekday(Sunday).toDate(today)    shouldBe new LocalDate(2014, 6, 29)
  }

  it should "interpret next month" in {

    import Month._

    val today =  new LocalDate(2014, 1, 1)

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
}
