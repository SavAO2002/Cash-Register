module Structures(Category(..), 
                  Bill(..), 
                  Fruit(..), 
                  Vegetable(..),
                  Drink(..),
                  Product(..),
                  Basket(..),
                  Card(..)) where

data Category = Fruit| Vegetable| Drink deriving (Eq, Show)

data Fruit = Apple| Banana| Peach| Orange| Limon| Pear| Cherry deriving Show
data Vegetable = Cucumber| Tomato| Peas| Onion| Lettuce| Pepper| Carrot deriving Show
data Drink = Water| Tea| Latte| Cappuchino| Juice| Limonade| Milk deriving Show

data Product = Product {
                           name :: String, 
                           prodCost :: Either String Double, 
                           category :: Either String Category
                        } deriving (Show) {-названиe, цена, категория-}

newtype Basket = Basket [Either String (Product, Int)] deriving (Show)
newtype Card = Card (String, Double) deriving (Show)

data Bill = Bill{
                products :: Basket,
                allCost :: Double,
                discount :: Double,
                costWithDiscount :: Double,
                info :: [String], {-отрицательные цены или количество товаров в корзине; 
                                    проценты скидки на бонусной карте, < 0 или > 7 %.-}
                notExist :: [String] {-список продуктов, у которых цена не указана-}
                } 
            deriving (Show)