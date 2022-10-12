module GameStatus ( GameStatus (..)
                  , PosPlayer (..)
                  , Pos (..)
                  , Map (..)
                  , Case (..)
                  , createGameStatus
                  , setCharPressed
                  , caseIsEmpty
                  , movePos
                  , refreshMap
                  , getMapString
                  , getCase
                  , setWin
                  , mergeGameStatus) where

import Control.Exception
import Exception (MyException (..))
import Args (Args (..))

data Pos = Pos Int Int deriving Show
instance Num Pos where
  (Pos xa ya) + (Pos xb yb) = Pos (xa + xb) $ ya + yb
  (Pos xa ya) - (Pos xb yb) = Pos (xa - xb) $ ya - yb

newtype PosPlayer = PosPlayer Pos deriving Show

showRow :: [Char] -> String
showRow row = '#':row ++"#\n"

generateBorders :: Int -> String
generateBorders x = replicate (x+2) '#' ++ "\n"

data Case = Case Char Pos deriving Show

data Map = Map Pos [[Char]]
instance Show Map where
  show (Map (Pos x _) rawmap) = concat $ [generateBorders x] ++ map showRow rawmap ++ [generateBorders x]

data GameStatus = GameStatus (Maybe Char) PosPlayer Map (Maybe Map) Bool deriving Show
--instance Show GameStatus where
--  show (GameStatus mc pos) = show mc

data MapInfo = MapInfo Map PosPlayer deriving Show

getMapString :: GameStatus -> String
getMapString (GameStatus _ _ _ (Just realmap) _) = show realmap
getMapString _                                   = throw $ MapNotRefreshed

setCharOnMap'' :: Int -> Char -> [Char] -> [Char]
setCharOnMap'' 0 c (_:xs)    = c:xs
setCharOnMap'' x c (mapc:xs) = mapc: setCharOnMap'' (x-1) c xs
setCharOnMap'' x _ _         = throw $ PosOutOfBoundX x

setCharOnMap' :: Int -> Int -> Char -> [[Char]] -> [[Char]]
setCharOnMap' x 0 c (maprow:xss) = (setCharOnMap'' x c maprow):xss
setCharOnMap' x y c (maprow:xss) = maprow:setCharOnMap' x (y-1) c xss
setCharOnMap' _ y _ _            = throw $ PosOutOfBoundY y

setCharOnMap :: Pos -> Char -> Map -> Map
setCharOnMap (Pos x y) c (Map mapsize rawmap) = Map mapsize . setCharOnMap' x y c $ rawmap

addPlayerToMap :: PosPlayer -> Char -> Map -> Map
addPlayerToMap (PosPlayer pos) c = setCharOnMap pos c

refreshMap :: GameStatus -> GameStatus
refreshMap (GameStatus mc pp rawmap _ win) = GameStatus mc pp rawmap (Just . addPlayerToMap pp 'P' $ rawmap) win

getMapSize' :: Int -> Int -> Bool -> [[Char]] -> Pos
getMapSize' y x True  (row:xs) = getMapSize' (y+1) x (x == length row) xs
getMapSize' y x True  []       = Pos x y
getMapSize' _ _ False _        = throw MapBuildSizeError

getMapSize :: [[Char]] -> Pos
getMapSize (row:xs) = getMapSize' 1 (length row) True xs
getMapSize []     = throw MapBuildError

replaceEmptyMap'' :: Char -> Char
replaceEmptyMap'' '#' = '#'
replaceEmptyMap'' 'E' = 'E'
replaceEmptyMap''  _  = ' '

replaceEmptyMap' :: [Char] -> [Char]
replaceEmptyMap' (x:xs)   = replaceEmptyMap'' x:replaceEmptyMap' xs
replaceEmptyMap' []       = []

replaceEmptyMap :: [[Char]] -> [[Char]]
replaceEmptyMap (x:xs) = replaceEmptyMap' x:replaceEmptyMap xs
replaceEmptyMap []      = []

createMap :: [[Char]] -> Map
createMap rawmap = Map (getMapSize rawmap) . replaceEmptyMap $ rawmap

mergeMapInfo :: MapInfo -> MapInfo -> MapInfo
mergeMapInfo (MapInfo newmap (PosPlayer (Pos newx newy))) (MapInfo oldmap (PosPlayer (Pos x y)))
  | newx /= -1 && newx /= x && newy /= -1 && newy /= y = throw $ MyException
mergeMapInfo newmi _ = newmi

createMapInfoCase :: Char -> Int -> Int -> MapInfo -> MapInfo
createMapInfoCase 'P' x y (MapInfo rawmap _) = MapInfo rawmap (PosPlayer (Pos x y))
createMapInfoCase _   _ _ mi              = mi

createMapInfoRow' :: [Char] -> Int -> Int -> MapInfo -> MapInfo
createMapInfoRow' (c:xs) x y mi = let newmi = createMapInfoCase c x y mi in mergeMapInfo (newmi) (createMapInfoRow' xs (x+1) y newmi)
createMapInfoRow' []     _ _ mi = mi

createMapInfoRow :: [Char] -> Int -> MapInfo -> MapInfo
createMapInfoRow row y mi = createMapInfoRow' row 0 y mi

createMapInfoMap' :: [[Char]] -> Int -> MapInfo -> MapInfo
createMapInfoMap' (row:xs) y mi = let newmi = createMapInfoRow row y mi in mergeMapInfo (newmi) (createMapInfoMap' xs (y+1) newmi)
createMapInfoMap' []       _ mi = mi

createMapInfoMap :: [[Char]] -> MapInfo -> MapInfo
createMapInfoMap rawmap mi = createMapInfoMap' rawmap 0 mi

createMapInfo :: [[Char]] -> MapInfo
createMapInfo rawmap = createMapInfoMap rawmap $ MapInfo (createMap rawmap) (PosPlayer $ Pos (-1) (-1))

createGameStatus' :: MapInfo -> GameStatus
createGameStatus' (MapInfo basemap pp) = GameStatus Nothing pp basemap Nothing False

createGameStatus :: Args -> GameStatus
createGameStatus (Args basemap) = createGameStatus' . createMapInfo $ basemap

setCharPressed :: GameStatus -> Maybe Char -> GameStatus
setCharPressed (GameStatus _ pp m currentmap win) mc = GameStatus mc pp m currentmap win

safeGetCharOfMap'' :: Int -> [Char] -> Maybe Char
safeGetCharOfMap'' 0 (c:_)  = Just c
safeGetCharOfMap'' x (_:xs) = safeGetCharOfMap'' (x-1) xs
safeGetCharOfMap'' x _      = Nothing

safeGetCharOfMap' :: Int -> Int -> [[Char]] -> Maybe Char
safeGetCharOfMap' 0 x (c:_)  = safeGetCharOfMap'' x c
safeGetCharOfMap' y x (_:xs) = safeGetCharOfMap' (y-1) x xs
safeGetCharOfMap' y _ _      = Nothing

safeGetCharOfMap :: Map -> Pos -> Maybe Char
safeGetCharOfMap (Map _ rawmap) (Pos x y) = safeGetCharOfMap' y x rawmap

getCharOfMap' :: Pos -> Maybe Char -> Char
getCharOfMap' _         (Just c) = return c
getCharOfMap' (Pos x y) Nothing  = throw $ PosOutOfBoundXY x y

getCharOfMap :: Map -> Pos -> Char
getCharOfMap currentmap p = getCharOfMap' p $ safeGetCharOfMap currentmap p

getCase'

getCase :: Maybe Map -> Pos -> Maybe Case
getCase (Just currentmap) p = Case (getCharOfMap currentmap p) p
getCase Nothing           _ = throw MyException

caseIsEmpty :: Map -> Pos -> Bool
caseIsEmpty m p = ' ' == getCharOfMap m p

movePos :: Pos -> Pos -> Pos
movePos pos movement = pos + movement

setWin :: GameStatus -> Bool -> GameStatus
setWin (GameStatus mc pp basemap currentmap _) win = GameStatus mc pp basemap currentmap win

mergeGameStatus :: GameStatus -> GameStatus -> GameStatus
mergeGameStatus
  newgamestatus@(GameStatus _ _ _ _ True)
  (GameStatus _ _ _ _ _)
              = newgamestatus
mergeGameStatus
  (GameStatus _ _ _ _ _)
  oldgamestatus@(GameStatus _ _ _ _ True)
              = oldgamestatus
mergeGameStatus
  newgamestatus@(GameStatus newmc newpp newbasemap newcurrentmap newwin)
  oldgamestatus@(GameStatus mc pp basemap currentmap win)
  | otherwise = newgamestatus