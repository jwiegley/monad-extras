-- | Delay the current thread with an unbound number of milliseconds.

module Control.Concurrent.Delay
  (delayMicrosecs
  ,delayMillisecs
  ,delaySeconds
  ,delayMinutes
  ,delayHours
  ,delayDays
  ) where

import Control.Concurrent

-- | Suspends the current thread for a given number of microseconds.
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run earlier than specified.
delayMicrosecs :: Integer -> IO ()
delayMicrosecs microsecs
  | microsecs <= fromIntegral maxMicrosecs = threadDelay (fromIntegral microsecs)
  | otherwise = do
    threadDelay maxMicrosecs
    delayMicrosecs (microsecs - fromIntegral maxMicrosecs)

  where maxMicrosecs = maxBound :: Int

-- | Delay the current thread for at least n milliseconds.
delayMillisecs :: Integer -> IO ()
delayMillisecs = delayMicrosecs . (*1000)

-- | Delay the current thread for at least n seconds.
delaySeconds :: Integer -> IO ()
delaySeconds = delayMillisecs . (* 1000)

-- | Delay the current thread for at least n minutes.
delayMinutes :: Integer -> IO ()
delayMinutes = delaySeconds . (*60)

-- | Delay the current thread for at least n hours.
delayHours :: Integer -> IO ()
delayHours = delayMinutes . (*60)

-- | Delay the current thread for at least n days.
delayDays :: Integer -> IO ()
delayDays = delayHours . (*24)
