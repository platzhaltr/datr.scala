package com

package object acme {

  type ParsedEvent   = Either[DateEvent, TimeEvent]
  type ParsedDuration = Either[DateDuration, TimeDuration]
  type ParsedCompound = Either[ParsedEvent, ParsedDuration]

}
