package com

package object acme {

  type SimpleEvent   = Either[DateEvent, TimeEvent]
  type ComplexEvent  = DurationEvent
  type CompoundEvent = Either[SimpleEvent, ComplexEvent]

}
