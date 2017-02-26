# README

```
sbt compile
sbt run
Enter expression >
```

## Supported formats

Some examples of supported date and time entries:

**Formal dates**

```
2014-08-20
20140820
on 2014-08-20
on 20140820
1984/04/02
on 1984/04/02
20/02/1980
1st August
first march
the 2nd of april
2nd August 2015
on 3rd Sep 2015
on 4th Dec
```

**Relaxed dates**

```
today
tom
tomorrow
yesterday
saturday
mon
tue.
last wednesday
last september
last week
last month
last year
next thursday
next week sat
next week
next month
next year
first saturday in september
second monday in december
second mon in dec
in 1 day
in 2 weeks
in 3 months
in 4 years
in one day
in two weeks
in three months
in four years
one day ago
two weeks ago
three months ago
four years ago
```

**Formal times**

```
5:00
16
16:00
at 2:00
at 5
5:30 a.m.
at 7 pm
```

**Relaxed times**

```
now
10 seconds ago
in 5 minutes
4 hours from now
```

**Fuzzy times**

```
noon
afternoon
midnight
```

**Durations**

```
for 10 seconds
for 3 minutes
for 6 hours
until 17
until 8pm
from 5 to 7
from 13:00 to 8:30
from 9:00 till 12:15
from 10pm to 12pm
for 7 days
for 3 days starting yesterday
for 5 days starting today
for 5 days starting tomorrow
for 7 days starting next monday
for 8 days starting in 5 weeks
for 8 days starting first saturday in april
till wednesday
from monday to wednesday
```

**Combinations**

```
tommorow afternoon
yesterday evening
tomorrow at 8 a.m.
yesterday at 10 pm
monday 12:00
tuesday at 5 a.m.
23. July 12:00
24. Dec 8 pm
24. Dec at 8 pm
in 2 days at 6 pm
in 4 weeks at 12:00
in 6 weeks at 20
in 3 months at 8 a.m.
in 1 year at 20:00
next week tue 5
first wednesday of march at 22:00
saturday afternoon
```

*Durations with relative dates* This is still experimental as I'm solving backtracking issues.

```
from 9:30 until 12:15 next sunday
from 14 to 15 on mon
```

## Resources

**Inspirations**

- [Natty](http://natty.joestelmach.com/)

**Recurring Events**

- RFC 5545, Internet Calendaring and Scheduling Core Object Specification (iCalendar)
- [Martin Fowler, Recurring Events for Calendars ](http://martinfowler.com/apsupp/recurring.pdf)
