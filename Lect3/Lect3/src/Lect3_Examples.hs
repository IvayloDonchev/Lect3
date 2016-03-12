module Lect3_Examples where

import Data.Char

-- проверява дали символът е цифра
isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

-- преобразуване на малки в главни букви
toUpper :: Char -> Char
toUpper ch
    | isLower ch = chr (ord ch + ord 'A' - ord 'a')
    | otherwise = ch
    
