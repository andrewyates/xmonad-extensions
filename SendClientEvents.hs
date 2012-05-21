-----------------------------------------------------------------------------
-- |
-- Pokes the TimeTracker every n seconds to export its data.
-- You have to run this program in order to get something exported from the TT.
-- I'll probably embed it into the TT soon.
-----------------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import System.Environment (getArgs)
import System.Console.GetOpt

import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Display
import Graphics.X11.Types

import XMonad.Actions.TimeTracker.Export (ttAtomName)

sendTimerEvent display rWindow messageTypeAtom = do
    print $ "Sending " ++ ttAtomName
    allocaXEvent $ \eventPtr -> do
         setEventType eventPtr clientMessage
         setClientMessageEvent eventPtr rWindow messageTypeAtom 32 0 currentTime
         sendEvent display rWindow False structureNotifyMask eventPtr
    sync display False

main = do
    interval <- fmap ( getInt . fromInterval . head ) parseArgs
    display <- openDisplay ""
    rWindow  <- rootWindow display $ defaultScreen display
    messageTypeAtom <- internAtom display ttAtomName False
    forever (sendTimerEvent display rWindow messageTypeAtom >> threadDelaySeconds interval)

threadDelaySeconds = threadDelay . (*10^6)

usageHeader = "Usage: runghc SendClientEvents [OPTION...]"
parseArgs = do
    args <- getArgs
    let ( flags, nonOpts, msgs ) = getOpt RequireOrder options args
    case getOpt RequireOrder options args of
      ([],    [],      [])     -> error $ printUsage []
      (flags, [],      [])     -> return flags
      (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
      (_,     _,       msgs)   -> error $ printUsage msgs

printUsage msgs = concat msgs ++ usageInfo usageHeader options

options :: [OptDescr Flag]
options = [ Option ['i'] ["interval"] (ReqArg Interval "SECONDS") "interval in seconds" ]

data Flag = Interval String deriving Show

fromInterval (Interval x) = x

getInt str = case reads str :: [(Int, String)] of
               [(i, "")] -> i
               _         -> error $ printUsage [str ++ " is not a valid Int.\n"]
