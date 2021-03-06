package org.platzhaltr.parsing.datr

import java.time._

import org.parboiled2._

trait ParseResult

class DateParser(val input: ParserInput) extends Parser {

  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[ParseResult] = rule {
    DateTimes                                  ~> ((d: DateEvent, t: TimeEvent) => DateTimeEvent(d,t)) |
    Times                                      ~> ((t: TimeEvent) => t)|
    Dates                                      ~> ((d: DateEvent) => d)  |
    TimeDurations                              ~> ((d: TimeDuration) =>  d) |
    DateDurations                              ~> ((d: DateDuration) => d)
  }

  def DateTimes = rule {
    AbsoluteDateTimes |
    RelativeDateTimes
  }

  def RelativeDateTimes = rule {
    RelativeDates ~ Space ~ AbsoluteTimes
  }

  def AbsoluteDateTimes = rule {
    AbsoluteDates ~ Space ~ !(YearDigits) ~ AbsoluteTimes
  }

  def Times = rule {
    AbsoluteTimes | RelativeTimes
  }

  def Dates = rule {
    AbsoluteDates | RelativeDates
  }

  def AbsoluteTimes = rule {
    FormalTimes | FuzzyTimes
  }

  def FormalTimes = rule {
    TwelveHourTime                             ~> ((t) => AtTime(t)) |
    IsoTime                                    ~> ((t) => AtTime(t)) |
    At ~ Space ~ TwelveHourTime                ~> ((t) => AtTime(t)) |
    At ~ Space ~ IsoTime                       ~> ((t) => AtTime(t))
  }

  def FuzzyTimes = rule {
    Noon                                       ~> ((t) => AtTime(t)) |
    Afternoon                                  ~> ((t) => AtTime(t)) |
    Evening                                    ~> ((t) => AtTime(t)) |
    Midnight                                   ~> ((t) => AtTime(t))
  }

  def RelativeTimes = rule {
    Now                                        ~> (()  => InSeconds(0)) |
    In ~ Space ~ Number ~ Space ~ Seconds      ~> ((s) => InSeconds(s)) |
    In ~ Space ~ Number ~ Space ~ Minutes      ~> ((m) => InMinutes(m)) |
    In ~ Space ~ Number ~ Space ~ Hours        ~> ((h) => InHours(h)) |
    Number ~ Space ~ Seconds ~ Space ~ Ago     ~> ((s) => InSeconds(-s)) |
    Number ~ Space ~ Minutes ~ Space ~ Ago     ~> ((m) => InMinutes(-m)) |
    Number ~ Space ~ Hours ~ Space ~ Ago       ~> ((h) => InHours(-h)) |
    Number ~ Space ~ Seconds ~ Space ~ FromNow ~> ((s) => InSeconds(s)) |
    Number ~ Space ~ Minutes ~ Space ~ FromNow ~> ((m) => InMinutes(m)) |
    Number ~ Space ~ Hours ~ Space ~ FromNow   ~> ((h) => InHours(h))
  }

  def AbsoluteDates = rule {
    optional((On|The) ~ Space) ~ (FormalDate | IsoDate | LittleEndianDate) ~> ((d) => OnDate(d))
  }

  def RelativeDates = rule {
    RelativeDays |
    RelativeDatesFuture |
    RelativeDatesPast |
    CardinalWeekdayInMonth
  }

  def CardinalWeekdayInMonth = rule {
    Cardinal ~ Space ~ SpecificWeekday ~ Space ~ (In | Of) ~ Space ~ SpecificMonth ~> ((c,w,m) => WeekdayInMonth(c, w, m))
  }

  def RelativeDays = rule {
    Today                                      ~> (()  => InDays(0)) |
    Tomorrow                                   ~> (()  => InDays(1)) |
    Yesterday                                  ~> (()  => InDays(-1))
  }

  def RelativeDatesFuture = rule {
    SpecificWeekday                            ~> ((w) => NextWeekdayByName(w)) |
    SpecificMonth                              ~> ((m) => NextMonthByName(m)) |
    Next ~ Space ~ Months                      ~> (()  => InMonths(1)) |
    Next ~ Space ~ Weeks ~ Space ~ SpecificWeekday ~> ((w) => NextWeekWeekdayByName(w)) |
    Next ~ Space ~ SpecificWeekday             ~> ((w) => NextWeekdayByName(w)) |
    Next ~ Space ~ SpecificMonth               ~> ((m) => NextMonthByName(m)) |
    Next ~ Space ~ Weeks                       ~> (()  => InWeeks(1)) |
    Next ~ Space ~ Years                       ~> (()  => InYears(1)) |
    On ~ Space ~ SpecificWeekday             ~> ((w) => NextWeekdayByName(w)) |
    In ~ Space ~ SpecificMonth                 ~> ((m) => NextMonthByName(m)) |
    In ~ Space ~ (Count | Number) ~ Space ~ Days         ~> ((d) => InDays(d)) |
    In ~ Space ~ (Count | Number) ~ Space ~ Weeks        ~> ((w) => InWeeks(w)) |
    In ~ Space ~ (Count | Number) ~ Space ~ Months       ~> ((m) => InMonths(m)) |
    In ~ Space ~ (Count | Number) ~ Space ~ Years        ~> ((y) => InYears(y))
  }

  def RelativeDatesPast = rule {
    Last ~ Space ~ Months                      ~> (()  => InMonths(-1)) |
    Last ~ Space ~ SpecificWeekday             ~> ((w) => LastWeekdayByName(w)) |
    Last ~ Space ~ SpecificMonth               ~> ((m) => LastMonthByName(m)) |
    Last ~ Space ~ Weeks                       ~> (()  => InWeeks(-1)) |
    Last ~ Space ~ Years                       ~> (()  => InYears(-1)) |
    Count ~ Space ~ Days ~ Space ~ Ago         ~> ((c) => InDays(-c)) |
    Count ~ Space ~ Weeks ~ Space ~ Ago        ~> ((c) => InWeeks(-c)) |
    Count ~ Space ~ Months ~ Space ~ Ago       ~> ((c) => InMonths(-c)) |
    Count ~ Space ~ Years ~ Space ~ Ago        ~> ((c) => InYears(-c))
  }

  def TimeDurations = rule {
    DurationPrefix ~ Seconds       ~> ForSeconds |
    DurationPrefix ~ Minutes       ~> ForMinutes |
    DurationPrefix ~ Hours         ~> ForHours |
    FromTime ~ Space ~ ToTime ~ Space ~ RelativeDates ~> ((f, t, d) => FromTimeToTime(f,t,Some(d))) |
    FromTime ~ Space ~ ToTime      ~> (FromTimeToTime(_,_, None)) |
    UnTill ~ Space ~ AbsoluteTimes ~> UntilTime
  }

  def FromTime = rule { From ~ Space ~ AbsoluteTimes }
  def ToTime = rule { (To | UnTill) ~ Space ~ AbsoluteTimes }

  def DateDurations = rule {
    DurationPrefix ~ Days ~ Space ~ Starting ~ Space ~ RelativeDates ~> ((d, e) => RelativeDateDuration(e,ForDays(d))) |
    DurationPrefix ~ Days                      ~> ForDays |
    From ~ Space ~ SpecificWeekday ~ Space ~ To ~ Space ~ SpecificWeekday ~> ((s,f) => RelativeDateDuration(NextWeekdayByName(s),UntilWeekday(f))) |
    UnTill ~ Space ~ SpecificWeekday             ~> ((w) => RelativeDateDuration(InDays(0),UntilWeekday(w)))
  }

  def DurationPrefix = rule {
    For ~ Space ~ Number ~ Space
  }

  def Cardinal     = rule { First | Second | Third | Fourth | Fifth}
  def First        = rule { (ignoreCase("first")  | ignoreCase("1st")) ~> (() => 1)}
  def Second       = rule { (ignoreCase("second") | ignoreCase("2nd")) ~> (() => 2)}
  def Third        = rule { (ignoreCase("third")  | ignoreCase("3rd")) ~> (() => 3)}
  def Fourth       = rule { (ignoreCase("fourth") | ignoreCase("4th")) ~> (() => 4)}
  def Fifth        = rule { (ignoreCase("fifth")  | ignoreCase("5th")) ~> (() => 5)}

  def Count        = rule { One | Two | Three | Four | Five }
  def One          = rule { ignoreCase("one")   ~> (() => 1)}
  def Two          = rule { ignoreCase("two")   ~> (() => 2)}
  def Three        = rule { ignoreCase("three") ~> (() => 3)}
  def Four         = rule { ignoreCase("four")  ~> (() => 4)}
  def Five         = rule { ignoreCase("five")  ~> (() => 5)}

  def Ago          = rule { ignoreCase("ago") }
  def At           = rule { ignoreCase("at") | "@" }
  def For          = rule { ignoreCase("for") }
  def From         = rule { ignoreCase("from") }
  def FromNow      = rule { From ~ Space ~ Now }
  def In           = rule { ignoreCase("in") }
  def Now          = rule { ignoreCase("now") }
  def Of           = rule { ignoreCase("of") }
  def On           = rule { ignoreCase("on") }
  def Starting     = rule { ignoreCase("starting") }
  def Till         = rule { ignoreCase("till") }
  def To           = rule { ignoreCase("to") }
  def Th           = rule { ignoreCase("th") }
  def The          = rule { ignoreCase("the") }
  def Until        = rule { ignoreCase("until") }
  def UnTill       = rule { Till | Until }

  def Last         = rule { ignoreCase("last") }
  def Next         = rule { ignoreCase("next") }

  def Today        = rule { ignoreCase("today") }
  def Tomorrow     = rule { ignoreCase("tom") ~ optional(ignoreCase("orrow")) }
  def Yesterday    = rule { ignoreCase("yesterday") }

  def Seconds      = rule { ignoreCase("second") ~ optional(ignoreCase("s")) }
  def Minutes      = rule { ignoreCase("minute") ~ optional(ignoreCase("s")) }
  def Hours        = rule { ignoreCase("hour")   ~ optional(ignoreCase("s")) }
  def Days         = rule { ignoreCase("day")    ~ optional(ignoreCase("s")) }
  def Weeks        = rule { ignoreCase("week")   ~ optional(ignoreCase("s")) }
  def Months       = rule { ignoreCase("month")  ~ optional(ignoreCase("s")) }
  def Years        = rule { ignoreCase("year")   ~ optional(ignoreCase("s")) }

  def Noon         = rule { ignoreCase("noon") ~> (() => LocalTime.of(12,0))}
  def Afternoon    = rule { ignoreCase("afternoon") ~> (() => LocalTime.of(16,0))}
  def Evening      = rule { ignoreCase("evening") ~> (() => LocalTime.of(19,0))}
  def Midnight     = rule { ignoreCase("midnight") ~> (() => LocalTime.of(0,0))}

  def Number       = rule { capture(Digits) ~> (_.toInt) }
  def Digits       = rule { oneOrMore(CharPredicate.Digit) }

  def Colon        = rule { ":" }
  def Dash         = rule { "-" }
  def Dot          = rule { "." }
  def Slash        = rule { "/" }
  def Space        = rule { zeroOrMore(" ") }

  def FormalDate   = rule {
   DayDigits ~ optional(Dot | Th) ~ Space ~ SpecificMonth ~ Space ~ YearDigits ~> ((d,m,y) => Right(LocalDate.of(y, m.getValue, d))) |
   DayDigits ~ optional(Dot | Th) ~ Space ~ SpecificMonth ~> ((d,m) => Left(MonthDay.of(m.getValue,d))) |
   Cardinal ~ Space ~ SpecificMonth ~ Space ~ YearDigits ~> ((d,m,y) => Right(LocalDate.of(y, m.getValue, d))) |
   Cardinal ~ Space ~ optional(Of) ~ Space ~ SpecificMonth ~> ((d,m) => Left(MonthDay.of(m.getValue,d)))
  }

  def IsoDate      = rule { YearDigits ~ optional(Dash | Slash) ~ MonthDigits ~ optional(Dash | Slash) ~ DayDigits ~> ((y,m,d) => Right(LocalDate.of(y, m, d))) }
  def LittleEndianDate = rule { DayDigits ~ optional(Dash | Slash) ~ MonthDigits ~ optional(Dash | Slash) ~ YearDigits ~> ((d,m,y) => Right(LocalDate.of(y, m, d))) }
  def YearDigits   = rule { capture(4.times(CharPredicate.Digit)) ~> (_.toInt) }
  def MonthDigits  = rule { capture("0" ~ CharPredicate.Digit | "1" ~ anyOf("012" )) ~> (_.toInt) }
  def DayDigits    = rule { capture(anyOf("012") ~ CharPredicate.Digit | "3" ~ anyOf("01" )) ~> (_.toInt) }

  def IsoTime      = rule {
    FullIsoTime | HoursOnly
  }

  def FullIsoTime  = rule { HourDigits ~ Colon ~ MinuteDigits ~> ((h,m) => LocalTime.of(h, m)) }
  def HoursOnly    = rule { HourDigits ~ &(Space ~ (To | Next | On |  EOI)) ~> ((h) => LocalTime.of(h, 0))  }

  def HourDigits   = rule { capture(anyOf("01") ~ optional(CharPredicate.Digit) | ("2" ~ optional(anyOf("01234" ))) | anyOf("3456789")) ~> (_.toInt) }
  def MinuteDigits = rule { capture(anyOf("012345") ~ optional(CharPredicate.Digit)) ~> (_.toInt) }

  def TwelveHourTime = rule {
    TwelveHour ~ Colon ~ MinuteDigits ~ Space ~ Am ~> ((h,m) => LocalTime.of(h % 12,m)) |
    TwelveHour ~ Colon ~ MinuteDigits ~ Space ~ Pm ~> ((h,m) => LocalTime.of((h % 12) + 12,m)) |
    TwelveHour ~ Am                     ~> ((h) => LocalTime.of(h % 12,0)) |
    TwelveHour ~ Pm                     ~> ((h) => LocalTime.of((h % 12) + 12,0)) |
    TwelveHour ~ Space ~ Am             ~> ((h) => LocalTime.of(h % 12,0)) |
    TwelveHour ~ Space ~ Pm             ~> ((h) => LocalTime.of((h % 12) + 12,0))
  }
  def TwelveHour = rule { capture("0" ~ CharPredicate.Digit | "1" ~ anyOf("012") | CharPredicate.Digit) ~> (_.toInt) }
  def Am         = rule { ignoreCase("a") ~ optional (Dot) ~ ignoreCase("m") ~ optional(Dot) }
  def Pm         = rule { ignoreCase("p") ~ optional (Dot) ~ ignoreCase("m") ~ optional(Dot) }

  def SpecificWeekday = rule {Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday}
  def Monday          = rule {(ignoreCase("monday")    | (ignoreCase("mon") ~ optional (Dot))) ~> (() => DayOfWeek.MONDAY )}
  def Tuesday         = rule {(ignoreCase("tuesday")   | (ignoreCase("tue") ~ optional (Dot))) ~> (() => DayOfWeek.TUESDAY )}
  def Wednesday       = rule {(ignoreCase("wednesday") | (ignoreCase("wed") ~ optional (Dot))) ~> (() => DayOfWeek.WEDNESDAY )}
  def Thursday        = rule {(ignoreCase("thursday")  | (ignoreCase("thu") ~ optional (Dot))) ~> (() => DayOfWeek.THURSDAY )}
  def Friday          = rule {(ignoreCase("friday")    | (ignoreCase("fri") ~ optional (Dot))) ~> (() => DayOfWeek.FRIDAY )}
  def Saturday        = rule {(ignoreCase("saturday")  | (ignoreCase("sat") ~ optional (Dot))) ~> (() => DayOfWeek.SATURDAY )}
  def Sunday          = rule {(ignoreCase("sunday")    | (ignoreCase("sun") ~ optional (Dot))) ~> (() => DayOfWeek.SUNDAY )}

  def SpecificMonth = rule {January | Febuary | March | April | May | June | July | August | September | October | November | December}
  def January       = rule {(ignoreCase("january")   | (ignoreCase("jan") ~ optional (Dot))) ~> (() => Month.JANUARY )}
  def Febuary       = rule {(ignoreCase("february")  | (ignoreCase("feb") ~ optional (Dot))) ~> (() => Month.FEBRUARY )}
  def March         = rule {(ignoreCase("march")     | (ignoreCase("mar") ~ optional (Dot))) ~> (() => Month.MARCH )}
  def April         = rule {(ignoreCase("april")     | (ignoreCase("apr") ~ optional (Dot))) ~> (() => Month.APRIL )}
  def May           = rule {(ignoreCase("may") ~ optional (Dot))                             ~> (() => Month.MAY )}
  def June          = rule {(ignoreCase("june")      | (ignoreCase("jun") ~ optional (Dot))) ~> (() => Month.JUNE )}
  def July          = rule {(ignoreCase("july")      | (ignoreCase("jul") ~ optional (Dot))) ~> (() => Month.JULY )}
  def August        = rule {(ignoreCase("august")    | (ignoreCase("aug") ~ optional (Dot))) ~> (() => Month.AUGUST )}
  def September     = rule {(ignoreCase("september") | (ignoreCase("sept") | ignoreCase("sep") ~ optional (Dot))) ~> (() => Month.SEPTEMBER )}
  def October       = rule {(ignoreCase("october")   | (ignoreCase("oct") ~ optional (Dot))) ~> (() => Month.OCTOBER )}
  def November      = rule {(ignoreCase("november")  | (ignoreCase("nov") ~ optional (Dot))) ~> (() => Month.NOVEMBER )}
  def December      = rule {(ignoreCase("december")  | (ignoreCase("dec") ~ optional (Dot))) ~> (() => Month.DECEMBER )}
}
