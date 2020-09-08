{-# LANGUAGE LambdaCase #-}

module Chapter3 (main) where

import Data.List
import qualified Chapter2 as C

-- Exercise 3-1

-- swapTriple :: (x,y,z) -> (y,z,x)
-- duplicate :: x -> (x,x)
-- nothing :: a -> Maybe b
-- index :: Num b => [a] -> [(b, a)]
-- maybeA :: [a] -> Char

-- Exercise 3-2

filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes = filter (\x -> x /= 1) 

filterANumber:: (Num b, Eq b) => b -> [b] -> [b]
filterANumber x = filter (\y -> y /= x) 

filterNot :: (Num a, Eq a) => (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f) 

filterGovOrgs :: [C.Client] -> [C.Client]
filterGovOrgs = filter (\x -> case isGov x of { True -> True; False -> False })
  where
    isGov :: C.Client -> Bool
    isGov (C.GovOrg _) = True
    isGov _ = False 

-- Exercise 3-3

product :: Num a => [a] -> a
product = foldr (*) 1

minimumClient :: [C.Client] -> C.Client
minimumClient = foldr1 (\x acc -> if (getLength x) < (getLength acc) then x else acc)
  where
    getLength :: C.Client -> Int
    getLength (C.GovOrg x) = length x 
    getLength (C.Company x _ _ _) = length x 
    getLength (C.Individual (C.Person first _ _) _) = length $ first 

all' :: [Bool] -> Bool
all' = foldr1 (&&)

-- Mini Exercise

elem' :: Eq a => a -> [a] -> Bool
elem' x ys = case find (\y -> x == y) ys of
               Just _ -> True
	       Nothing -> False

-- Exercise 3-4

mapAsFold :: (a -> b) -> [] a -> [] b
mapAsFold f = foldr (\x acc -> (f x) : acc) []

main :: IO ()
main = do
  putStrLn $ show $ filterOnes [1,2,3]
  putStrLn $ show $ filterANumber 2 [1,2,3]
  putStrLn $ show $ filterNot (\x -> x == 5) [1,5,3]
  putStrLn $ show $ filterGovOrgs C.testDataClient
  putStrLn $ show $ minimumClient C.testDataClient
  putStrLn $ show $ all' [True, True, False]
  putStrLn $ show $ all' [True, True, True]
  putStrLn $ show $ elem' 1 [1,2,3]
  putStrLn $ show $ elem' 0 [1,2,3]
  putStrLn $ show $ mapAsFold (\x -> x*2) [1,2,3]


