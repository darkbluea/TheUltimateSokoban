module Tools (getCharPressed) where

import UI.HSCurses.Curses

getCharPressed' :: Key -> Maybe Char
getCharPressed' (KeyChar c) = Just c
getCharPressed' _           = Nothing

getCharPressed :: IO (Maybe Char)
getCharPressed = getCh >>= return . getCharPressed'