module Time where

import Warning
import DClockTime
import DCalendarTime

toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime ct = warning "Time.toCalendarTime: not implemented"
                      (return (error "sorry, can't continue."))

