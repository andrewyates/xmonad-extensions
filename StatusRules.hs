-----------------------------------------------------------------------------
-- |
-- A simple example of a custom status extractor for the XMonad time tracker.
--
-- Sample usage:
-- 
-- import StatusRules
-- ...
-- myConfig = defaultConfig { ...
--    ...
--    ...
--    } `addTimeTrackerHook` defaultTTConfig { getStatusString = getCustomStatusString}
--       ...
-----------------------------------------------------------------------------

module StatusRules (getCustomStatusString) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Util.NamedWindows (getName)

getCustomStatusString :: X String
getCustomStatusString = do
    winset <- gets windowset
    windowTitle <- maybe (return "") (fmap show . getName) . S.peek $ winset
    workspaceTitle <- return . show . S.currentTag $ winset
    return $ customRules windowTitle workspaceTitle

customRules wt ws
    | wt  == "" = "Nothing"
    | wt `endsWith` "Youtube - Chromium" = "Watching Youtube"
    | wt `endsWith` "HaskellWiki - Chromium" = "Reading Haskell Wiki"
    | wt `startsWith` "Google Reader" && wt `endsWith` "- Chromium" = "Reading google reader"
    | wt `startsWith` "BBC News" && wt `endsWith` "- Chromium" = "Reading news"
    | wt `startsWith` "Twitter " && wt `endsWith` "- Chromium" = "Reading twitter"
    | wt `endsWith` "Gmail - Chromium" = "Email"
    | wt `endsWith` "- Chromium" = "Browsing elsewhere"
    | wt == "Terminal" || wt == "xterm" = "Terminal"
    | otherwise = "Workspace " ++ ws -- logging only current workspaces by default

startsWith :: String -> String -> Bool
startsWith str = and . zipWith (==) str

endsWith :: String -> String -> Bool
endsWith str suffix = startsWith (reverse str) (reverse suffix)
