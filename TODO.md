# TODO

## Prefixes

Locale specific holidays?

```
day after
the day before
the monday after
the monday before
2 fridays before
4 tuesdays after
```

## Repeats

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
```

## Durations

```
for 7 days
for 7 days starting next monday
from monday to wednesday
from monday 12 am to tuesday 4 pm
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
