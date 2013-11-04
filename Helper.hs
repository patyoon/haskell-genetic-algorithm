-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
TupleSections,  FlexibleContexts #-}

module Helper (char2bin,
               swap,
               twoTupleM,
               threeTupleM,
               repeatWhile,
               toBinRep,
               map2,
               isOrdered
              ) where

import Data.List (unfoldr)
import Control.Monad
-- | Helper function for checking if a list is ordered
isOrdered :: Ord b => (a -> b) -> [a] -> Bool
isOrdered f (x1:x2:xs) = f x1 <= f x2 && isOrdered f (x2:xs)
isOrdered _ _          = True

-- | repeat until a condition is met. Used for 
-- repeatedly evolving the population.
repeatWhile :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
repeatWhile stop f = go
  where go x | stop x = return x
             | otherwise = f x >>= go

-- | Helper function : make a two-tuple by replicating a monad instance twice
twoTupleM :: Monad m => m a -> m (a, a)
twoTupleM = liftM toTwoTuple . replicateM 2 
  where toTwoTuple :: [a] -> (a, a)
        toTwoTuple [x,y] = (x,y)
        toTwoTuple _     = error "Impossible case"

-- | Helper function : make a three-tuple by replicating a monad instance 
-- three times
threeTupleM :: Monad m => m a -> m (a, a, a)
threeTupleM = liftM toThreeTuple . replicateM 3
         where toThreeTuple :: [a] -> (a, a, a)
               toThreeTuple [x, y, z] = (x, y, z)
               toThreeTuple _         = error "Impossible case."

-- | helper swap function for mutateChSwap. 
-- Swaps element of a list at two indices.
swap :: Int -> Int -> [a] -> [a]
swap i j xs 
  |  i == j   = xs
  | otherwise = take l xs ++ [xs !! u] ++ 
                (take (u-l-1) $ drop (l+1) xs) ++ [xs !! l] ++ drop (u+1) xs
                where l = if i < j then i else j
                      u = if i > j then i else j

-- | Converts a character into its binary represenation
-- reference : PLEAC-Haskell
char2bin :: Char -> [Char]
char2bin = map i2c . reverse . unfoldr decomp . fromEnum
  where decomp :: Int -> Maybe (Int, Int)
        decomp n = if n == 0 then Nothing else Just(n `mod` 2, n `div` 2)
        i2c :: Int -> Char
        i2c i = if i == 0 then '0' else '1'

-- | Convert to binary representation of the String
toBinRep :: String -> String
toBinRep = concat . map char2bin

-- | map over two lists
-- reference : hw1 solution 
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
map2 _ []     _      = []
map2 _ _      []     = []