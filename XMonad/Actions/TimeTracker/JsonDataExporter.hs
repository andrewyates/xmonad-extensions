{-# LANGUAGE OverloadedStrings #-}
module XMonad.Actions.TimeTracker.JsonDataExporter (jsonDataExporter)
where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L

import Text.JSON

import XMonad.Actions.TimeTracker.Aggregator

jsonDataExporter :: String -> TTData -> IO Bool
jsonDataExporter url = trySendJsonData url . eventsToJson 

eventTupleToJson :: (EventName, (Integer, Maybe Integer)) -> JSObject JSValue
eventTupleToJson (name, (started, ended)) =
    toJSObject [("name", JSString $ toJSString name),
                ("started_at", JSString $ toJSString $ show started),
                ("ended_at", JSString $ toJSString $ showEnded ended)]
    where showEnded Nothing    = "Never"
          showEnded (Just end) = show end

dropCurrentEvent [] = []
dropCurrentEvent events@[(_, (_, Just _))] = events
dropCurrentEvent events = tail events

eventsToJson :: TTData -> JSValue
eventsToJson events = JSArray $ map ( JSObject . eventTupleToJson) $ dropCurrentEvent events

jsonToByteString json = case readJSON json :: Result L.ByteString of
                            Ok bs     -> bs
                            Error str -> L.empty -- Big-big TODO

trySendJsonData :: String -> JSValue -> IO Bool
trySendJsonData url jsonData = do
    req <- (parseUrl url) 
    let request = req { method = "POST"
                      , requestHeaders = [("Content-Type", "application/json")]
                      , requestBody = RequestBodyLBS $ jsonToByteString jsonData
                      }

    res <- withManager $ httpLbs req

    L.putStrLn $ responseBody res
    return True



