module CPUTime where

import Warning

getCPUTime       :: IO Integer
cpuTimePrecision :: Integer

getCPUTime        = warning "CPUTime.getCPUTime: not implemented" (return 0)
cpuTimePrecision  = warning "CPUTime.cpuTimePrecision: not implemented" 0

