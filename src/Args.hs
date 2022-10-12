module Args ( Args (..)
            , createArgs) where

import Control.Exception
import Exception (MyException (..))

data Args = Args [[Char]]

createArgs :: [String] -> IO Args
createArgs (path:[]) = readFile path >>= return . Args . lines
createArgs args      = throw $ ArgsError args