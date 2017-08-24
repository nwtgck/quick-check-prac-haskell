{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--(from: http://itpro.nikkeibp.co.jp/article/COLUMN/20080304/295346/?rt=nocnt)

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

prop_RevRevWithCollect xs = collect (length xs) (prop_RevRev xs)
  where types = xs :: [Int]

newtype MyInt = MyInt Int deriving (Eq, Ord, Show, Num)

instance Arbitrary MyInt where
  arbitrary = sized $ \n -> do
   v <- choose (-n, n)
   return (MyInt v)

propMyInt (MyInt n) = n < 100

data RGB = Red | Blue | Green deriving Show

instance Arbitrary RGB where
  arbitrary = oneof (fmap return [Red, Blue, Green])

prop_RGB rgb = True
  where types = rgb :: RGB


data Circuit a =
      Pure a
    | Not a
    | And2 (Circuit a) (Circuit a)
    | Or2  (Circuit a) (Circuit a)
    | And3 (Circuit a) (Circuit a) (Circuit a)
    | And4 (Circuit a) (Circuit a) (Circuit a) (Circuit a)
    | And5 (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a)
    | And6 (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a)
    | And7 (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a)
    | And8 (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a) (Circuit a)
    deriving Show

instance Arbitrary a => Arbitrary (Circuit a) where
  arbitrary = oneof [
      liftM Pure arbitrary
    , liftM Not arbitrary
    , liftM2 And2 arbitrary arbitrary
    , liftM3 And3 arbitrary arbitrary arbitrary
    , liftM4 And4 arbitrary arbitrary arbitrary arbitrary
    , liftM5 And5 arbitrary arbitrary arbitrary arbitrary arbitrary
    , And6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- liftM6 isn't defined so create by own
    , And7 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , And8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
   ]

prop_Circuit cur = True
  where types = cur :: Circuit Bool

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

  quickCheck propMyInt

  quickCheck prop_RGB

  quickCheck prop_Circuit

  when False $
    quickCheck prop_RevRevWithCollect

  when False $
    verboseCheck prop_negative2

  when False $
    quickCheckWith stdArgs{maxSuccess=100000} prop_not_always_correct
