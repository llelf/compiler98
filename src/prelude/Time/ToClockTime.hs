module Time where

import Warning
import DClockTime
import DCalendarTime

toClockTime :: CalendarTime -> ClockTime
toClockTime ct = warning "Time.toClockTime: not implemented" (CT 0)

