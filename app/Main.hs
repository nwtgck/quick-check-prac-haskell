module Main where

import           Control.Monad
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

prop2 x = x < 1
  where types = x :: Int

prop_negative2 x = (x <= 0) ==> (x * (-1) >= 0)
  where types = (x :: Int)

prop_RevRevWithClassify xs =
    (len == 0)  `classify` "empty list"
  $ (len == 1)  `classify` "one-element list"
  $ (len <= 10) `classify` "short list"
  $ prop_RevRev xs
  where
    len = length xs
    types = (xs :: [Int])

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

  quickCheck (forAll (fmap abs arbitrary) prop2)

  quickCheck prop_RevRevWithClassify

  when False $
    verboseCheck prop_negative2

  when False $
    quickCheckWith stdArgs{maxSuccess=100000} prop_not_always_correct
