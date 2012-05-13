module XMonad.Actions.TimeTracker.Export (
    TTDataExporter,
    addTimeTrackerExportHook,
    ttAtomName
  )
  where
import XMonad
import qualified XMonad.Util.ExtensibleState as XS

import Data.Maybe
import Control.Monad(when)
import qualified Data.Monoid as Mon

import XMonad.Actions.TimeTracker.Aggregator
import XMonad.Actions.TimeTracker

type TTDataExporter = TTData -> IO Bool

exportTTEventHook exporter e = do
    handleClientEvent e (exportTTData exporter)
    return (Mon.All True)

addTimeTrackerExportHook xConfig exporter = 
    xConfig { handleEventHook = exportTTEventHook exporter <+> handleEventHook xConfig }

ttAtomName = "XMONAD_GET_TIME_TRACKER_DATA"

----

exportTTData :: TTDataExporter -> X ()
exportTTData exporter = do
    aggregator <- XS.get :: X Aggregator
    res <- io $ exporter $ recentEvents aggregator
    when True $ XS.put $ cleanAggregator aggregator

handleClientEvent :: Event -> X () -> X ()
handleClientEvent (ClientMessageEvent {ev_message_type = mt}) action = do
    d <- asks display
    a <- io $ internAtom d ttAtomName False
    when (mt == a) action
handleClientEvent _ _ = return ()

