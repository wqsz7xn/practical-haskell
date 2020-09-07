{-# LANGUAGE NamedFieldPuns #-}

module Chapter2 (main) where

-- Exercise 2-1

rewrite1 :: [[Char]]
rewrite1 = ('a':'b':'c':[]):(('d':'e':[]):[])

rewrite2 :: [Char]
rewrite2 = 'a':'b':'c':[]

rewrite3 :: [Char]
rewrite3 = 'a':[]

rewrite4 :: [Char]
rewrite4 = []

isEmpty :: [[a]] -> Bool
isEmpty [] = True
isEmpty xs = case (head xs) of 
               [] -> True
               _  -> False 

oneElement :: [a] -> Bool
oneElement (x:[]) = True
oneElement     _  = False

concat' :: Monoid a => [a] -> a
concat' = foldr (\a acc -> a <> acc) mempty 

-- Exercise 2-2
-- Already done as this project I guess

-- Exercise 2-3
-- Although these are not like the ones in the book they should work.
-- If you don't understand look at someone elses answers. These have been tested in Chapter2.main

reverse2 :: String -> String
reverse2 = reverse

(+++) :: Monoid a => a -> a -> a
a +++ b = a <> b

-- Exercise 2-4

data Person =
  Person String String Gender
  deriving (Eq, Show)

data Gender =
    Male
  | Female
  | Other String
  deriving (Eq, Show)

data Client =
    GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)

data TimeMachine =
  TimeMachine {
    manufacturer :: String,
    model :: Integer,
    timeType :: TimeType,
    price :: Double
  }
  deriving (Eq, Show)

data TimeType =
    Future
  | Past
  deriving (Eq, Show)

-- Exercise 2-5

clientsEachGender :: (Num a) => [Client] -> (a, a, a)
clientsEachGender = foldr go (0,0,0)
  where
    go :: (Num a) => Client -> (a,a,a) -> (a,a,a)
    go x (a, b, c) = case x of
	               Company _ _ (Person _ _ Male) _ -> (a+1,b,c)
	               Company _ _ (Person _ _ Female) _ -> (a, b+1,c)
	               Company _ _ (Person _ _ (Other _)) _ -> (a, b,c+1)
	               Individual (Person _ _ Male) _ -> (a+1, b,c)
	               Individual (Person _ _ Female) _ -> (a, b+1,c)
	               Individual (Person _ _ (Other _)) _ -> (a, b,c+1)
		       _ -> (a,b,c)

testDataClient :: [Client]
testDataClient = 
    [ (Company "" 0 (Person "" "" $ Other "they") "")
    , (Company "" 0 (Person "" "" $ Other "they") "")
    , (Company "" 0 (Person "" "" $ Other "they") "")
    , (Company "" 0 (Person "" "" $ Other "they") "")
    , (Company "" 0 (Person "" "" Male) "")
    , (Company "" 0 (Person "" "" Female) "")
    , (Individual (Person "" "" Male) False)
    ]

timeMachineDiscount :: TimeMachine -> Double -> TimeMachine
timeMachineDiscount (TimeMachine a b c d) discount = 
  TimeMachine a b c (discount * d)

testTimeMachine = TimeMachine "" 5 Future 50.0

-- Exercise 2-6

ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | m > 0 && n > 0 = ackermann (m - 1) (ackermann m n-1) 

unzip' :: [(a,a)] -> ([a], [a])
unzip' ((a,b):(c,d):_) = ([a,b], [c,d])

data PersonR = PersonR { firstName :: String
                       , lastName :: String
		       } deriving (Show)


-- Views
greet :: PersonR -> String
greet PersonR { firstName } = "yo! " <> firstName

-- Exercise 2-7
-- I had already written the TimeMachine type using Records. Please see above to see the answer

timeMachineDiscount' :: TimeMachine -> Double -> TimeMachine
timeMachineDiscount' t@(TimeMachine { price = p } ) discount = 
  t { price = (p*discount) }

main :: IO ()
main = do
  putStrLn $ show rewrite1
  putStrLn $ show rewrite2
  putStrLn $ show rewrite3
  putStrLn $ show rewrite4
  putStrLn $ show $ isEmpty rewrite1
  putStrLn $ show $ isEmpty [[]]
  putStrLn $ show $ isEmpty [['a']]
  putStrLn $ show $ oneElement['a']
  putStrLn $ show $ oneElement "abc"
  putStrLn $ show $ concat ["abc", "de"]
  putStrLn $ show $ reverse2 "abcde"
  putStrLn $ show $ "abc"+++ "de"
  putStrLn $ show $ [1,2,3]+++ [1,2,3]
  putStrLn $ show $ Company "" 0 (Person "" "" $ Other "they") ""
  putStrLn $ show $ clientsEachGender testDataClient
  putStrLn $ show $ clientsEachGender testDataClient
  putStrLn $ show $ timeMachineDiscount testTimeMachine 0.5
  putStrLn $ show $ ackermann 0 5 
  putStrLn $ show $ unzip' [(1,2),(3,4)]
  putStrLn $ show $ greet $ PersonR "wqsz7xn" "ab" 
  putStrLn $ show $ timeMachineDiscount' testTimeMachine 0.5
