-- Amin Babar
-- 11/09/18
--ghc 7.10
-- Function usaChange calculates the minimum amount of coins needed of each
-- denomination given a certain amount. 
-- Function  change takes a list of positive Integers representing Haskelland’s coin
-- denominations, and an Integer x representing the monetary value of the change that needs to
-- be made. and returns a list of integers indicating how many coins of each
-- denomination to return.
-- Collaborated with Amr Abdelhady

import Data.List (sortBy)
import Data.Function (on)


-- Calculates the number of coins used for each denomination in a coins list by using div and mod.
-- Calculates recursively so that it can keep track of n. Greedy Algorithm used. List returned by
-- this function will be referred to as coinsList in the rest of the program.
coinsUsed :: Int -> [Int] -> [Int]
coinsUsed n (x:xs) 
    | n == 0 = []
    | x == 1 = [1] ++ (coinsUsed (n - 1) [x])
    | otherwise = (take (n `div` x) $ repeat x) ++ coinsUsed (n `mod` x) xs


-- Calculates the frequency of one element in the denom list in the coinsList by using filter
-- (with partially applied function) on coinsList.
frequency :: [Int] -> Int  -> Int
frequency coinsList element = length $ filter (== element) coinsList


-- Ensures the output is in the right format by calculating the frequency of each denomination
-- in the coinsList. Denom denotes denomination list.
formatCorrecter :: [Int] -> [Int] -> [Int]
formatCorrecter [] coinsList = []
formatCorrecter denom coinsList = [frequency coinsList (head denom)] ++ formatCorrecter (tail denom) coinsList


-- Calculates the coins needed using greedy algorithm given a certain amount of change for USA coin
-- denominations.
usaChange amount = formatCorrecter [25, 10, 5, 1] $ coinsUsed amount [25, 10, 5, 1]


-- Creates lists of all different combinations given a certain input for coin denominations. Either an
-- element is in the list or its not in the list. Calculates all combinations using that fact.
combinations :: [Int] -> [[Int]]
combinations (x:xs)
    |xs == [] = [[x]]
    |otherwise = map (\sublist -> x: sublist) (restcomb)  ++ restcomb
    where restcomb = combinations xs


-- applies the greedy algorithm to all the combinations of lists. Finds the list with the minimum length
-- and outputs that list in the right format.
change :: [Int] -> Int -> [Int]
change x n = formatCorrecter x (head (sortBy (compare `on` length) listelems)) 
    where listelems = map (coinsUsed n) (combinations x)
    
