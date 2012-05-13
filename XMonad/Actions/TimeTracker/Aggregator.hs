{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Actions.TimeTracker.Aggregator 
    (Aggregator,
     EventName,
     TimeFrame,
     TTData,
     aggregateEvent,
     unidleAggregator,
     createNewAggregator,
     setTimeout,
     recentEvents,
     cleanAggregator)
where

import Data.List (sort, reverse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

type TimeInSeconds = Integer

type TimeFrame = (Integer, Maybe Integer)

type EventName = String

type TTData = [(EventName, TimeFrame)]

data Aggregator = Aggregator { lastActiveTime :: Maybe TimeInSeconds
                             , timeout        :: TimeInSeconds
                             , recentEvents   :: TTData } deriving (Show, Typeable)

aggregateEvent name time aggr@Aggregator { recentEvents = [] } =
    addEv aggr (name, (time, Nothing))

aggregateEvent name time aggr@Aggregator { recentEvents = (_, (_, Just _)):restEv } =
    addEv aggr (name, (time, Nothing))

aggregateEvent name time aggr@Aggregator { recentEvents = (curr, (startTime, Nothing)):restEv }
    | curr /= name = aggr `closeTopFrame` time `addEv` (name, (time, Nothing))
    | True = aggr

addEv aggr ev = aggr { recentEvents = ev:(recentEvents aggr) }

closeTimeFrame now Nothing        _  start = (start, Just now)
closeTimeFrame now (Just lastAct) to start = (start, Just $ min (lastAct + to) now)

closeTopFrame aggr@Aggregator { recentEvents = [] } _ = aggr
closeTopFrame aggr@Aggregator { recentEvents = (curr, (startTime, Nothing)):restEv } now =
    aggr { recentEvents = (curr, closeTimeFrame now (lastActiveTime aggr) (timeout aggr) startTime):restEv}

unidleAggregator time aggr
    | not $ isIdle (timeout aggr) time (lastActiveTime aggr) = aggr { lastActiveTime = Just time }
    | True = aggr { lastActiveTime = Just time } `closeTopFrame` time

createNewAggregator = Aggregator { lastActiveTime = Nothing, recentEvents = [], timeout = 5*60 }

cleanAggregator aggr@Aggregator { recentEvents = [] } = aggr
cleanAggregator aggr@Aggregator { recentEvents = (_, (_, Just _)):_ } = aggr { recentEvents = [] }
cleanAggregator aggr@Aggregator { recentEvents = topFrame:_ } = aggr { recentEvents = [topFrame] }

setTimeout sec aggr =  aggr { timeout = sec }

isIdle _ _ Nothing = False
isIdle idleTime now (Just lastAct) = (now - lastAct) > idleTime

