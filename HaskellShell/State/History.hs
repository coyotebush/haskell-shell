module HaskellShell.State.History where
import Data.Foldable (toList)
import qualified Data.Sequence as Seq

type HistoryEntry = (Int, String)
data History = History Int (Seq.Seq HistoryEntry)
               deriving Show

emptyHistory :: History
emptyHistory = History 0 Seq.empty

addToHistory :: History -> Maybe Int -> String -> History
addToHistory (History n xs) d x = History (n + 1) $ maybeTake d $ (n + 1, x) Seq.<| xs
                                  where maybeTake Nothing    = id
                                        maybeTake (Just num) = Seq.take num

getHistory :: History -> [HistoryEntry]
getHistory (History _ xs) = toList $ Seq.reverse xs


