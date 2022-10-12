module GameLoop (gameLoop) where

import UI.HSCurses.Curses
import Tools (getCharPressed)
import GameStatus ( GameStatus (..)
                  , PosPlayer (..)
                  , Pos (..)
                  , Map (..)
                  , Case (..)
                  , setCharPressed
                  , caseIsEmpty
                  , movePos
                  , refreshMap
                  , getMapString
                  , getCase
                  , setWin
                  , mergeGameStatus)

testExit :: GameStatus -> Maybe (Int, String)
testExit (GameStatus (Just 'L') _ _ _ _)    = Just (0, "Normal Exit")
testExit (GameStatus _          _ _ _ True) = Just (1, "You Won")
testExit _                                  = Nothing

rawMovePlayer'' :: Pos -> Pos -> Bool -> PosPlayer
rawMovePlayer'' p _    False = PosPlayer p
rawMovePlayer'' _ newp True  = PosPlayer newp

rawMovePlayer' :: Pos -> Map -> Pos -> PosPlayer
rawMovePlayer' p gmap@(Map (Pos mapx mapy) _) newp@(Pos newx newy)
  | newx < 0      = PosPlayer p
  | newy < 0      = PosPlayer p
  | newx >= mapx  = PosPlayer p
  | newy >= mapy  = PosPlayer p
  | otherwise     = rawMovePlayer'' p newp . caseIsEmpty gmap $ newp

rawMovePlayer :: PosPlayer -> Map -> Pos -> PosPlayer
rawMovePlayer (PosPlayer p) gmap movement = rawMovePlayer' p gmap . movePos p $ movement

getPlayerMovement :: Maybe Char -> Pos
getPlayerMovement (Just 'q') = Pos (-1) 0
getPlayerMovement (Just 'd') = Pos (1)  0
getPlayerMovement (Just 'z') = Pos 0    (-1)
getPlayerMovement (Just 's') = Pos 0    (1)
getPlayerMovement _          = Pos 0    0

movePlayer :: GameStatus -> GameStatus
movePlayer (GameStatus mc pp m currentmap win) = GameStatus mc (rawMovePlayer pp m $ getPlayerMovement mc) m currentmap win

startActionOnCase :: GameStatus -> Case -> GameStatus
startActionOnCase gamestatus@(GameStatus (Just 'e') _ _ _ _) (Case 'E' _) = setWin gamestatus True
startActionOnCase gamestatus                                 _            = gamestatus

startAction' :: GameStatus -> [Case] -> GameStatus
startAction' gamestatus (x:xs) = let newgamestatus = startActionOnCase gamestatus x in mergeGameStatus newgamestatus $ startAction' newgamestatus xs
startAction' gamestatus []     = gamestatus

startAction :: GameStatus -> GameStatus
startAction gamestatus@(GameStatus _ (PosPlayer p) _ currentmap _) = startAction' gamestatus . map (getCase currentmap . movePos p) $ [ getPlayerMovement $ Just 'q'
                                                                                                                                      , getPlayerMovement $ Just 'z'
                                                                                                                                      , getPlayerMovement $ Just 'd'
                                                                                                                                      , getPlayerMovement $ Just 's']

useCharPressed :: GameStatus -> GameStatus
useCharPressed gamestatus@(GameStatus (Just 'z') _ _ _ _) = movePlayer gamestatus
useCharPressed gamestatus@(GameStatus (Just 'q') _ _ _ _) = movePlayer gamestatus
useCharPressed gamestatus@(GameStatus (Just 's') _ _ _ _) = movePlayer gamestatus
useCharPressed gamestatus@(GameStatus (Just 'd') _ _ _ _) = movePlayer gamestatus
useCharPressed gamestatus                                 = startAction gamestatus

gameLoop :: GameStatus -> Window -> Maybe (Int, String) -> IO (Maybe (Int, String))
gameLoop gamestatus w Nothing = do gamestatus <- return . refreshMap $ gamestatus
                                   mvWAddStr w 0 0 . getMapString $ gamestatus
--                                   mvWAddStr w 10 10 . show $ gamestatus
                                   refresh
                                   wclear w
                                   gamestatus <- (getCharPressed >>= return . useCharPressed . setCharPressed gamestatus)
                                   gameLoop gamestatus w . testExit $ gamestatus
gameLoop _          _ ec      = return ec