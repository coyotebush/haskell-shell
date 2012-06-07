module HaskellShell.State.History where
import Data.Foldable (toList)
import qualified Data.Sequence as Seq

type HistoryEntry = String
data History = History Int (Seq.Seq String)
               deriving Show

emptyHistory :: Int -> History
emptyHistory d = History d Seq.empty

addToHistory :: History -> String -> History
addToHistory (History d xs) x = History d $ Seq.take d $ x Seq.<| xs

getHistory :: History -> [HistoryEntry]
getHistory (History _ xs) = toList $ Seq.reverse xs


