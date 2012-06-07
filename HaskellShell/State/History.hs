module HaskellShell.State.History where
import Data.Foldable (toList)

type HistoryEntry = (Int, String)
data History = History Int [HistoryEntry]
               deriving Show

emptyHistory :: History
emptyHistory = History 0 []

addToHistory :: History -> Maybe Int -> String -> History
addToHistory (History n xs) d x = History (n + 1) $ maybeTake d $ (n + 1, x):xs
                                  where maybeTake Nothing    = id
                                        maybeTake (Just num) = take num

getHistory :: History -> [HistoryEntry]
getHistory (History _ xs) = reverse xs


