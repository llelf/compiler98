module DbgIface where

fatal :: a -> String -> a
fatal _ s = error s

blackhole = "Black hole detected in eval ZAP!"
