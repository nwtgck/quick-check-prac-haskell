module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Property

-- reverse check
prop_RevRev xs = reverse (reverse xs) == xs

-- Not always true, but normal quickCheck always pass
prop_not_always_correct :: Int -> Bool
prop_not_always_correct n = n < 100

prop_always_wrong x = x+1 < x
  where types = x :: Int

prop_1 x y = x+y < x
  where types = (x::Int, y::Int)

prop_negative x = x * (-1) < 0
  where types = (x :: Int)

prop_negative2 x = (x <= 0) ==> (x * (-1) >= 0)
  where types = (x :: Int)

main :: IO ()
main = do
  quickCheck (prop_RevRev :: [Int] -> Bool)
  quickCheck (prop_RevRev :: [Char] -> Bool)
  quickCheck (prop_RevRev :: [String] -> Bool)
  quickCheck prop_not_always_correct

  quickCheck prop_always_wrong
  quickCheck prop_1

  quickCheck prop_negative
  quickCheck prop_negative2

  -- quickCheckWith stdArgs{maxSuccess=100000} prop_not_always_correct
