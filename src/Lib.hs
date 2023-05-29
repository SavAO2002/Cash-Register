{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Lib (printB, convertInBasket, convertInCard) where

import Structures
import Data.List.Split

convertInCard:: String -> Either String Card --функция для получения скидочной карты
convertInCard text = case (createCard $ words text) of 
                    Right r -> Right (Card r)
                    Left l -> Left l

createCard :: [String] -> Either String (String, Double) --вспомогательная функция для получения скидочной карты
createCard [] = Left "Create card error"
createCard [birthday, discountCard] 
  | 7 > discPercent && discPercent >= 0 = Right (birthday, discPercent)
  | otherwise = Left "Create card error: percent < 0 or > 7 %"
    where discPercent = (read discountCard :: Double)
createCard _ = Left "Create card error"

convertInBasket:: [String] -> [String] -> Basket --функция для получения корзины
convertInBasket [] _ = Basket []
convertInBasket bask infoProd = Basket (map (\line -> createProduct (words line) infoProd) bask)

createProduct :: [String] -> [String] -> Either String (Product, Int) --вспомогательная функция для получения продукта
createProduct ["Product:", nameP, "Number:", num] infoProd = 
    case inInfo nameP infoProd of
      Just [_, categoryP, price] -> Right withP
        where withP = (Product nameP (Right priceD) (getCategory categoryP), numD)
              priceD = (read price:: Double)
              numD = read num :: Int
      Just [_, categoryP] -> Right withoutP
        where withoutP = (Product nameP (Left "No price") (getCategory categoryP), numD)
              numD = read num :: Int
      Just _ -> Left "Invalid format of product line"
      Nothing -> Left "Invalid format of product line or no product"
createProduct _ _ = Left "Invalid format of line in basket"

inInfo :: String -> [String] -> Maybe [String] --вспомогательная функция для поиска продуктов в списке магазина
inInfo _ [] = Nothing
inInfo nameP (x:xs) | nameP == (head strInf) = Just strInf
                    | otherwise = inInfo nameP xs
                        where strInf = words $ x

getCategory :: String -> Either String Category --вспомогательная функция для получения категории товара
getCategory nameP = case nameP of
                    "Apple" -> Right Fruit
                    "Banana" -> Right Fruit
                    "Peach" -> Right Fruit
                    "Orange"-> Right Fruit
                    "Lemon" -> Right Fruit
                    "Pear" -> Right Fruit
                    "Cherry" -> Right Fruit
                    "Cucumber" -> Right Vegetable
                    "Tomato" -> Right Vegetable
                    "Peas" -> Right Vegetable
                    "Onion" -> Right Vegetable
                    "Lettuce" -> Right Vegetable
                    "Pepper" -> Right Vegetable
                    "Carrot" -> Right Vegetable
                    "Water" -> Right Drink
                    "Tea" -> Right Drink
                    "Latte" -> Right Drink
                    "Cappuchino" -> Right Drink
                    "Juice" -> Right Drink
                    "Limonade" -> Right Drink
                    "Milk" -> Right Drink
                    _ -> Left "Invalid category in product line"

convertInBill:: Basket -> Card -> String -> Bill -- функция для создания счета
convertInBill b c date = Bill b cost d costD inf notex
                         where 
                            cost = basketCost b
                            costD = cost * (1 - d)
                            d = discountCost b c date cost
                            inf = negative b
                            notex = exist b

discountCost:: Basket -> Card -> String -> Double -> Double -- вспомогательная для convertInBill; подсчет цены с учетом скидки
discountCost (Basket b) (Card (birthday, num)) date d = allDisc
  where allDisc = (num + disBirthday birthday date + dis1 d + dis2 b) / 100

disBirthday:: String -> String -> Double --условие для скидки на день рождения
disBirthday birthday date
  | birthday == "" = 0
  | (birthS !! 1) == (dateS !! 1) && (birthS !! 0) == (dateS !! 0) = 3
  | otherwise = 0
    where birthS = (splitOn "." birthday)
          dateS = (splitOn "." date)

dis1 :: Double -> Double --условие для скидки по цене
dis1 d | d > 1000 = 3
       | otherwise = 0

dis2 :: [Either String (Product, Int)] -> Double --условие для скидки по количеству продуктов категории "Напиток"
dis2 sList | (foldr (+) (0::Int) (map func (onlyProd sList))) >= (3::Int) = 3.0
           | otherwise = 0.0
  where func = (\((Product _ _ cat), _) -> if (cat == Right Drink) 
                                           then (1::Int) else (0::Int))

onlyProd::[Either String (Product, Int)] -> [(Product, Int)] --вспомогательная функция для dis2
onlyProd [] = []                                             --вовращает список продуктов без ошибок
onlyProd (x:xs) = case x of 
  Left _ -> onlyProd xs
  Right (Product _ _ (Left _), _) -> onlyProd xs
  Right (Product p c (Right r), n) -> (Product p c (Right r), n) : (onlyProd xs)

basketCost:: Basket -> Double -- вспомогательная для convertInBill; подсчет цены без учета скидки
basketCost (Basket []) = 0
basketCost (Basket ((Left _):xs)) = basketCost (Basket xs)
basketCost (Basket (Right ((Product _ (Right cost) _), n):xs)) 
            |(cost < 0) || (n < 0) = basketCost (Basket xs)
            |otherwise = cost * (fromIntegral n) + basketCost (Basket xs)
basketCost (Basket (Right ((Product _ (Left _) _), _):xs)) = basketCost (Basket xs)

negative:: Basket -> [String] --подсчет ошибок в цене и количестве
negative (Basket []) = []
negative (Basket ((Left l):xs)) = l: negative (Basket xs)
negative (Basket (Right((Product s (Right cost) _), n):xs)) 
            | (cost < 0) = ("Cost " ++ s ++ " is negative") : negative (Basket xs)
            | (n < 0) = sNeg : negative (Basket xs)
            | otherwise = negative (Basket xs)
            where sNeg = ("Number of things - " ++ s ++ " - is negative")
negative (Basket (_:xs)) = negative (Basket xs)

exist:: Basket -> [String] --подсчет количества продуктов без указанной цены
exist (Basket []) = []
exist (Basket (Right ((Product s (Left _) _), _):xs)) = s : exist (Basket xs)
exist (Basket (Right ((Product _ (Right _) _), _):xs)) = exist (Basket xs)
exist (Basket (Left _:xs)) = exist (Basket xs)

printB:: Basket -> Either String Card -> String -> Either [String] Bill --вывод чека или ошибок
printB b cardEither date = case cardEither of
                           Left l -> Left [l]
                           Right c -> ifErrors bill
                            where bill = convertInBill b c date

ifErrors :: Bill -> Either [String] Bill --проверка на ошибки
ifErrors (Bill products_b cost_b disc_b cost_d_b err1 err2) 
      | (err1 == []) = Right (Bill products_b cost_b disc_b cost_d_b err1 err2)
      | otherwise = Left (err1 ++ err2)
