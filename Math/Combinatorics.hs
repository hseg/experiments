module Math.Combinatorics
  ( both
  , choose
  , doubleFactorial
  , factorial
  , multiFactorial
  , matching
  , std
  , weight
  )
where

import           Math.Combinat.Partitions.Integer
                                                ( Partition
                                                , fromPartition
                                                )

choose :: [a] -> Int -> [[a]]
choose (x : xs) n | n > 0 = map (x :) (xs `choose` (n - 1)) ++ xs `choose` n
choose xs _               = [ [] | null xs ]

std :: (Num a, Enum a) => a -> [a]
std n = [0 .. n - 1]

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

weight :: Partition -> Int
weight = sum . fromPartition

-- Might want to export some kind of lens exposing a list's length, under iso
-- std↔fromIntegral.length

matching :: (Num a, Ord a) => a -> a
matching = doubleFactorial . subtract 1

multiFactorial :: (Num a, Ord a) => a -> a -> a
multiFactorial k = product . takeWhile (>= k) . iterate (subtract k)

factorial, doubleFactorial :: (Num a, Ord a) => a -> a
factorial = multiFactorial 1
doubleFactorial = multiFactorial 2
