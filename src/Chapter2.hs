module Chapter2 (main) where

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
