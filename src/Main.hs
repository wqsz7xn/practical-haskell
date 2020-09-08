module Main where

import qualified Chapter2 (main)
import qualified Chapter3 (main)

main :: IO ()
main = do
  Chapter2.main
  Chapter3.main
