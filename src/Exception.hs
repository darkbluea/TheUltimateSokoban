module Exception ( MyException (..)
                 , handleException) where

import Control.Exception (Exception)

data MyException = MyException
                 | MyStringException String
                 | ArgsError [String]
                 | PosOutOfBoundX Int
                 | PosOutOfBoundY Int
                 | PosOutOfBoundXY Int Int
                 | MapBuildError
                 | MapBuildSizeError
                 | MapNotRefreshed deriving Show
instance Exception MyException

handleException :: MyException -> IO (Maybe (Int, String))
handleException ex = return . Just $ (1, show ex)