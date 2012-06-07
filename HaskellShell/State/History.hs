module HaskellShell.State.History where
import Data.Foldable (toList)
import qualified Data.Sequence as Seq

type HistoryEntry = (Int, String)
data History = History Int Int (Seq.Seq HistoryEntry)
               deriving Show

emptyHistory :: Int -> History
emptyHistory d = History d 0 Seq.empty

addToHistory :: History -> String -> History
addToHistory (History d n xs) x = History d (n + 1) $ Seq.take d $ (n + 1, x) Seq.<| xs

getHistory :: History -> [HistoryEntry]
getHistory (History _ _ xs) = toList $ Seq.reverse xs


