module XMonad.Actions.TimeTracker.JsonDataExporter (jsonDataExporter)
where

import Text.JSON

import XMonad.Actions.TimeTracker.Aggregator

jsonDataExporter :: TTData -> IO Bool
jsonDataExporter = trySendJsonData . eventsToJson 

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

eventsToJson :: TTData -> String
eventsToJson events = encode $ JSArray $ map ( JSObject . eventTupleToJson) $ dropCurrentEvent events

--- Not sending it anywhere, just dropping into a file - TODO
filename = "/tmp/a.log"
trySendJsonData :: String -> IO Bool
trySendJsonData jsonData = do appendFile filename $ jsonData ++ "\n"
                              return True

