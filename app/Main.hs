module Main (main) where

import Control.Exception
import System.Environment (getArgs)
import Args ( Args
            , createArgs)
import System.Exit (exitSuccess, ExitCode (ExitFailure), exitWith)
import UI.HSCurses.Curses
import GameStatus (createGameStatus)
import GameLoop (gameLoop)
import Exception (handleException)

startLoop :: Window -> Args -> IO (Maybe (Int, String))
startLoop w args = gameLoop (createGameStatus args) w Nothing

finishProgram :: Maybe (Int, String) -> IO ()
finishProgram Nothing       = exitSuccess
finishProgram (Just (0, _)) = exitSuccess
finishProgram (Just (i, _)) = exitWith $ ExitFailure i

main :: IO ()
main = do initCurses
          echo False
          returnCode <- (initScr >>= (\w -> getArgs >>= createArgs >>= (startLoop w))) `catch` handleException
          endWin
          putStrLn . show $ returnCode
          finishProgram returnCode
