{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Executes the given action when you return after inactivity.
--
-- Sample usage:
--
-- import XMonad.Actions.IdleActions
-- ...
-- myConfig = defaultConfig { ...
--    ...
--    ...
--    } `addUnidleAction` (60*10, io $ spawn "xmessage Where have you been?")
-----------------------------------------------------------------------------
module XMonad.Actions.IdleActions (
    addUnidleAction,
    unIdleHook,
    isIdleX )
where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Monoid as Mon

import Control.Monad(when)
import System.Time(getClockTime, ClockTime(TOD))

type TimeSinceEpoch = Integer
type TimeSpan = Integer -- in seconds

data LastActiveTime = LastActiveTime TimeSinceEpoch | NeverBeenActive deriving Typeable

instance ExtensionClass LastActiveTime where
    initialValue = NeverBeenActive

addUnidleAction :: XConfig l -> (TimeSpan, X()) -> XConfig l
addUnidleAction conf (timeoutSec, action) =
    conf { handleEventHook = unIdleHook timeoutSec action <+> handleEventHook conf }

isIdleX :: TimeSpan -> X Bool
isIdleX timeoutSec = do lastTime <- XS.get :: X LastActiveTime
                        now <- io getEpochSeconds
                        return $ isIdle lastTime timeoutSec now

-- TODO: this is still far from being perfect. If all you are doing is moving a mouse
--       within one window, not crossing its borders, not pressing any keys or buttons,
--       this module will assume idleness after the timeout.
unIdleHook :: TimeSpan -> X () -> Event -> X Mon.All
unIdleHook _ _ ClientMessageEvent{} = return (Mon.All True) -- Ignoring ClientMessageEvents
unIdleHook timeoutSec action _ = unIdle timeoutSec action >> return (Mon.All True)

unIdle :: TimeSpan -> X () -> X ()
unIdle timeoutSec action = do lastTime <- XS.get :: X LastActiveTime
                              now <- io getEpochSeconds
                              XS.put $ LastActiveTime now
                              when (isIdle lastTime timeoutSec now) action

isIdle :: LastActiveTime -> TimeSpan -> TimeSinceEpoch -> Bool
isIdle NeverBeenActive _ _ = False
isIdle (LastActiveTime t) timeSpan now = now - t > timeSpan

getEpochSeconds :: IO TimeSinceEpoch
getEpochSeconds = do TOD seconds _ <- getClockTime -- In Haskell 98, where the ClockTime type is abstract, this will not work.
                     return seconds                -- Alternatively, we could use timegm from MissingH here.

