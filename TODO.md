# TODO

## Technical

* have test suite with a reference date and only have simple, but a lot of, integration tests over the parsed dates

## Duckling corpus

[duckling](https://github.com/wit-ai/duckling) is an amazing Clojure library and pretty much achieves what I wanted to achieve with this little grammar. They went and build a stochastic parsing engine that you can train. I have to look into that.

I really like their approach, the code and the names.

They created an abstract parser that returns various types and it's up to the developer to extract what they need.

```
;; => [{:label :time
;;        :start 15
;;        :end 49
;;        :value {:type "value", :value "2015-01-26T06:00:00.000-02:00", :grain :hour}
;;        :body "last Monday of January 2015 at 6am"}]
```

The return type also allows for highlighting of the extracted part.

They are not so constrained by type safety as me, but maybe I can incorporate some of their naming. I like

* grain
* period
* shift-duration

The idea of a cycle vs duration. A cycle is something that repeats itself eg. `this week`, will have a different meaning this week and next week.

For now here are some things that work for them but not for me:

```
now
mon.
Mon, February 18
the 1st of march
first of march
march first
march 3
march 3 2015
march 3rd 2015
march third 2015
3/3/2015
3/3/15
2015-3-3
on the 15
on the 15th
the 15th of february
15 of february
february the 15th
february 15
2/15
on 2/15
February 15
Aug 8
October 2014
10/31/1974
10/31/74
10-31-74
April 14, 2015
14th April 15
friday after next
next March (should result in month)
March after next
Sunday, Feb 10
Wed, Feb13
Monday, Feb 18
this week
this quarter
next quarter
last quarter
4th quarter 2018
sunday from last week
last week's sunday
wednesday of next week
friday after next
the day after tomorrow
the day before yesterday
last Sunday of March 2014
last Monday of March
the week of october 6th
last day of october 2015
third tuesday of september 2014
third tuesday after christmas 2014

3 in the AM
3 oclock am
at three am
3:18a
@ 3pm
3 oclock pm
3 o'clock in the afternoon
at 15 past 3pm
half three
a quarter to noon
at 7:30 PM on Fri, Sep 20
at 9am on Saturday
on Saturday for 9am
Fri, Jul 18, 2014 07:00 PM

in a sec
in one minute
in half an hour
in 2.5 hours
in one hourj
in 1h
a day from now
3 years from today
this summer
this winter
xmas
christmas
new year's eve

this past weekend
this week-end

by the end of next month
by the end of next week
by EOD
by EOM
after lunch
from 9:30 - 11:00 on Thursday
July 13-15
July 13 to 15
July 13 thru 15
July 13 through 15
July 13 - July 15
Aug 8 - Aug 12
9:30 - 11:00
```

## Durations

What do you expect as a result, with input like

`next week`

The whole week or in one week?

What is `yesterday evening`. Shouldn't it be a duration?

## Location based dates and times

next "holiday"

Locale specific holidays?

```
day after
the day before
the monday after
the monday before
2 fridays before
4 tuesdays after
```

Location specific times

```
sunrise
at sunrise
at sunset
two hours after sunrise
one hour before sunset
```

## Recurring Events

```
"every 10 minutes"
"every day at 6"
"every 6 weeks"
"every 6 weeks starting next week thursday"
"every second monday"
"every 3. monday"
"every first of september"
"every second monday of december"
"every 1. of sep"
"every 1. sep"
"every day at 8pm for 60 minutes"
```

**Implementation Ideas**

I haven't found a library that supports recurring events.

RecurringEvent
 - frequency
 - repeatability
 - firstOccurence

Frequency
  - basic (minute, hour, daily, weekly, monthly, yearly)
  - special (cardinal weekday)

Repeatability
  - until specific date
  - number of times

## Date Durations

```
next sunday at 7 pm for three hours
```

## Fuzzy times ##

Don't hard-code the times for `afternoon`, `morning`, and so on. Inject from outside.

```
night
```

## Focus ##

Depending on the usage the user might think of different things when saying `tuesday`. Maybe he is keeping track of bills. Naturally this will all (or at lost most of them will be in the past. So instead of looking at `tuesday` as the next Tuesday the `DateParse` should look into the past.

The same can be said about time. If you are just tracking things you tend to  look in the past.

So this needs to be configurable.

## Grace ##

In some cases it might make sense to add a grace period to the time interpretation.  Let's say the time is `17:58`. If you then input `18:00`, you probably mean tomorrow at `18:00` and not today.

At the moment the grace period is effectively `0`, adding one and making it even configurable should not be that hard.

## Fuzzy Dates ##

I'm not yet sure about what to to with fuzzy dates like

```
next weekend
```

I see two solutions:

1. Give back a duration of the whole weekend
2. Have a bias for a specific day
