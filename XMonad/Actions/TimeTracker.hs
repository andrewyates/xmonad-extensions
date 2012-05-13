{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Aggregates information about the current state of XMonad.
-- To export that information, you have to add an exporter.
--
-- Sample usage:
--
-- import XMonad.Actions.TimeTracker
-- import XMonad.Actions.TimeTracker.Export
-- import XMonad.Actions.TimeTracker.LogFileDataExporter
-- ...
-- myConfig = defaultConfig { ...
--    ...
--    ...
--    } `addTimeTrackerHook` defaultTTConfig
--      `addTimeTrackerExportHook` (logFileDataExporter "/tmp/tt-log.log")
-----------------------------------------------------------------------------
module XMonad.Actions.TimeTracker (
  addTimeTrackerHook,
  defaultTTConfig,
  TimeTrackerConfig(..)
  )
  where

import System.Time
import System.Locale
import qualified Data.Monoid as Mon

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Util.NamedWindows (getName)

import XMonad.Actions.TimeTracker.Aggregator
import XMonad.Actions.IdleActions (addUnidleAction)
import qualified XMonad.Util.ExtensibleState as XS

instance ExtensionClass Aggregator where
    initialValue = createNewAggregator

-- | Readonly configuration.
data TimeTrackerConfig = TimeTrackerConfig
    { idleTime        :: Integer  -- ^ timeout in seconds
    , getStatusString :: X String -- ^ gets current status
    }

-- | Default config for the time tracker. Uses comma-separated current window and workspace names
-- as a status. Assumes idleness after 5 minutes of inactivity.
defaultTTConfig = TimeTrackerConfig
    { idleTime = 5*60
    , getStatusString = getDefaultStatusString
    }

-- | Modifies XConfig to start the time tracker.
addTimeTrackerHook xConfig ttConfig =
    xConfig 
        { logHook = logHook xConfig <+> updateTimeTracker (getStatusString ttConfig)
        , startupHook = setAggrTimeout (idleTime ttConfig) <+> startupHook xConfig
        }
        `addUnidleAction` (idleTime ttConfig, unIdle)

-------------
setAggrTimeout sec = do aggregator <- XS.get :: X Aggregator
                        XS.put $ setTimeout sec aggregator

updateTimeTracker getStatus = do
    statusStr <- getStatus
    time <- io getEpochSeconds
    aggregator <- XS.get :: X Aggregator
    XS.put $ aggregateEvent statusStr time aggregator

unIdle = do time <- io $ getEpochSeconds
            aggregator <- XS.get :: X Aggregator
            XS.put $ unidleAggregator time aggregator

getEpochSeconds = do TOD seconds _ <- getClockTime
                     return seconds

getDefaultStatusString :: X String
getDefaultStatusString = do
    winset <- gets windowset
    windowTitle <- maybe (return "") (fmap show . getName) . S.peek $ winset
    workspaceTitle <- return . show . S.currentTag $ winset
    return $ windowTitle ++ ", " ++ workspaceTitle

