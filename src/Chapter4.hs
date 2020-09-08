{-# LANGUAGE LambdaCase #-}
module Chapter4 where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.List (unfoldr)
import System.Random

-- Exercise 4-1
-- This has been done as this repository itself

-- Exercise 4-2

testMap = M.fromList [("key1", 1), ("key2", 2)]
insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert' k v m = M.alter (const (Just v)) k m

delete' :: Ord k => k -> M.Map k a -> M.Map k a
delete' k m = M.alter (const Nothing) k m

adjust' :: Ord k => k -> a -> M.Map k a -> M.Map k a
adjust' k v m = M.alter (const $ Just v) k m

-- Exercise 4-3

-- Script from online for generating clients

clients :: Int -> Int -> [Client Integer]
clients count seed = zipWith
    assignId
    (unfoldr (Just . client) (mkStdGen seed))
    [1..count]

client 
  :: RandomGen g 
  => g 
  -> (Client Integer, g)
client g = case randomR (0 :: Int, 2) g of
    (0, g') -> (defaultGovOrg, g')
    (1, g') -> (defaultCompany, g')
    (_, g') -> (defaultIndividual, g')

assignId 
    :: Client Integer 
    -> Int 
    -> Client Integer
assignId c i = c { clientId = toInteger i }

defaultGovOrg :: Client Integer
defaultGovOrg = GovOrg 0 "govorg"

defaultCompany :: Client Integer
defaultCompany = Company 
    0 "company" defaultPerson "duty"

defaultIndividual :: Client Integer
defaultIndividual = Individual 
    0 "" 
 
defaultPerson :: Person
defaultPerson = Person "fn" "ln" Male

data Person =
  Person String String Gender
  deriving (Eq, Show, Ord)

data Gender =
    Male
  | Female
  | Other String
  deriving (Eq, Show, Ord)

data Client i =
    GovOrg { clientId :: i, clientName :: String } 
  | Company { clientId :: i, clientName :: String
            , person :: Person, duty :: String } 
  | Individual { clientId :: i, clientName :: String } 
  deriving (Show, Eq, Ord)

data ClientKind =
    GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Show, Eq, Ord)

clientToKind x = case x of
  (GovOrg {}) -> GovOrgKind
  (Company {}) -> CompanyKind
  (Individual {}) -> IndividualKind

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients =
  L.foldl'
    ( \acc client ->
        M.alter
          ( \case
              Nothing -> Just $ S.singleton client
              Just set -> Just $ S.insert client set
          )
          (clientToKind client)
          acc
    )
    M.empty

classifyClients' :: [Client Integer] -> Map ClientKind (S.Set (Client Integer))
classifyClients' xs = M.fromList $ go <$> xs
  where
    go :: (Client Integer) -> (ClientKind , S.Set (Client Integer))
    go x = case x of
      (GovOrg {}) -> (GovOrgKind, S.singleton x)
      (Company {}) -> (CompanyKind, S.singleton x)
      (Individual {}) -> (IndividualKind, S.singleton x)

main :: IO ()
main = do
  putStrLn $ show $ insert' "key3" 3 testMap
  putStrLn $ show $ delete' "key1" testMap
  putStrLn $ show $ adjust' "key1" 10 testMap
  putStrLn $ show $ classifyClients $ clients 10 5
  putStrLn $ show $ classifyClients' $ clients 10 5
