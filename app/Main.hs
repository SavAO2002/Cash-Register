{-# OPTIONS_GHC -Wall -Werror #-}
module Main (main) where

import Lib
import Structures
import IO_Functions
import Data.Time
import System.Environment

main :: IO ()
main = do
    nameFiles <- getArgs
    str <- getCurrentTime
    let (year, month, day)  = toGregorian.utctDay $ str 
    if null nameFiles then do
        putStrLn "No arguments"
    else do
        {-products, basket, card-}
        fstFile <- readFile $ head nameFiles
        scdFile <- readFile $ (nameFiles !! 1)
        if length nameFiles == 3 then do {-если информация о карте есть-}
            cardInfo <- readFile $ (nameFiles !! 2)
            printAll $ printB (convertInBasket (lines $ scdFile) (lines $ fstFile)) 
                              (convertInCard cardInfo) (show(day)++"."++show(month)++"."++show(year))
        else if length nameFiles == 2 then do {-если информации о карте нет-}              
            printAll $ printB (convertInBasket (lines $ scdFile) (lines $ fstFile)) 
                              (Right (Card ("", 0))) (show(day)++"."++show(month)++"."++show(year)) 
        else do
            putStrLn "Need more files"
    return()