module HaskellShell.State.Environment (initializeEnvironment, readEnv) where
import System.Posix.Env

initializeEnvironment :: IO ()
initializeEnvironment = mapM_ (\(k, v) -> setEnv k v False) defaultEnvironment

defaultEnvironment = [ ("HISTSIZE", "10" )
                     , ("PS1",      ">> ")
                     , ("PS2",      "> " )
                     ]

readEnv :: Read a => String -> IO (Maybe a)
readEnv k = getEnv k >>= return . maybeMaybeRead

maybeMaybeRead :: Read a => Maybe String -> Maybe a
maybeMaybeRead Nothing  = Nothing
maybeMaybeRead (Just s) = case reads s of
                            [(x, "")] -> Just x
                            _         -> Nothing

