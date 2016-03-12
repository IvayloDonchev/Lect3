module Lect3_Examples where

import Data.Char

-- проверява дали символът е цифра
isDig :: Char -> Bool
isDig ch = ('0' <= ch) && (ch <= '9')

-- преобразуване на малки в главни букви
toUpp :: Char -> Char
toUpp ch
    | isLower ch = chr (ord ch + ord 'A' - ord 'a')
    | otherwise = ch
    
