module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Property

-- reverse check
prop_RevRev xs = reverse (reverse xs) == xs

-- Not always true, but normal quickCheck always pass
prop_not_always_correct :: Int -> Bool
prop_not_always_correct n = n < 100

main :: IO ()
main = do
  quickCheck (prop_RevRev :: [Int] -> Bool)
  quickCheck (prop_RevRev :: [Char] -> Bool)
  quickCheck (prop_RevRev :: [String] -> Bool)
  quickCheck prop_not_always_correct
  quickCheckWith stdArgs{maxSuccess=100000} prop_not_always_correct
