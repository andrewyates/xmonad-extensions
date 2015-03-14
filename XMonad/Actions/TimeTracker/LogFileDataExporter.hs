module XMonad.Actions.TimeTracker.LogFileDataExporter (logFileDataExporter)
where

import XMonad.Actions.TimeTracker.Aggregator

logFileDataExporter :: FilePath -> TTData -> IO Bool
logFileDataExporter file events = 
    mapM_ (appendFile file . ppTuple) (reverse $ dropCurrentEvent events) >> return True

dropCurrentEvent [] = []
dropCurrentEvent events@[(_, (_, Just _))] = events
dropCurrentEvent events = tail events

ppTuple (name, (started, maybeEnded)) =
    name ++ "\SOH" ++ show started ++ "\SOH" ++ ppEnded ++ "\n"
    where ppEnded = case maybeEnded of
                        Nothing -> "Never"
                        Just a  -> show a


