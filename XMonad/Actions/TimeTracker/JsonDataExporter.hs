{-# LANGUAGE OverloadedStrings #-}
module XMonad.Actions.TimeTracker.JsonDataExporter (jsonDataExporter)
where

import Control.Monad.Trans (liftIO)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Text.JSON
import qualified Data.ByteString.Lazy.Char8 as L

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

trySendJsonData :: String -> JSValue -> IO Bool
trySendJsonData url jsonData = do
    withBody $ \body ->
      case parseUrl url of
        Nothing -> return False -- Invalid URL
        Just req -> withManager $ \manager -> do
            let request = req { method = "POST"
                              , requestHeaders = [("Content-Type", "application/json")]
                              , requestBody = RequestBodyLBS body
                              }
            Response status _ _ _ <- httpLbs request manager
            liftIO $ return $ status == created201
    where withBody action = case readJSON jsonData :: Result L.ByteString of
                                 Ok bs     -> action bs
                                 Error str -> return False
