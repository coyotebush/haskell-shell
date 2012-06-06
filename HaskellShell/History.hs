module HaskellShell.History where
import qualified Data.Sequence as Seq

data History = History { depth :: Int
                       , entries :: (Seq.Seq String)
                       }
               deriving Show

emptyHistory :: Int -> History
emptyHistory d = History { depth = d, entries = Seq.empty }

addToHistory :: History -> String -> History
addToHistory h s = h { entries = Seq.take (depth h) $ entries h Seq.|> s }


