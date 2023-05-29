{-# OPTIONS_GHC -Wall -Werror #-}
module IO_Functions(printAll) where

import Structures

printAllWarn:: [String] -> IO () --вывод продуктов без цены
printAllWarn [] = putStrLn "| Warning: these products are not in the receipt"
printAllWarn (x:xs) = do
                      printAllWarn xs
                      putStrLn ("| " ++ x)

printAll:: Either [String] Bill -> IO () --вывод чека или предупреждения о невозможности напечатать чек
printAll (Left []) = do
                   putStrLn "| The receipt cannot be calculated"
                   putStrLn "| Warnings:"
printAll (Left (x:xs)) = do
                       printAll (Left xs)
                       putStrLn ("| " ++ x)
printAll (Right (Bill (Basket []) firstCost disc lastCost _ warningList)) = do
            putStrLn ("-----------------------------------------------------")
            putStrLn ("| Cost - " ++ show firstCost)
            putStrLn ("| Discount - " ++ show disc)
            putStrLn ("| Cost with discount - " ++ show lastCost)
            putStrLn ("-----------------------------------------------------")
            if null warningList then do
                putStrLn ""
            else do
                printAllWarn warningList
printAll (Right (Bill (Basket (x:xs)) c d cd err1 err2)) = 
    case x of
    Left _ -> printAll (Right (Bill (Basket xs) c d cd err1 err2))
    Right (Product nameP cost _, countP) ->
            case cost of
            Left _ -> printAll (Right (Bill (Basket xs) c d cd err1 err2))
            Right dCost -> do
                putStrLn ("| " ++ nameP ++ " " ++ show dCost ++ " " ++ 
                         show countP ++ " = " ++ show (dCost * fromIntegral(countP)))
                printAll (Right (Bill (Basket xs) c d cd err1 err2))
