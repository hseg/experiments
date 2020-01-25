{-# LANGUAGE TupleSections #-}

module Math.Combinatorics
  ( both
  , choose
  , decomps
  , doubleFactorial
  , factorial
  , ive
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

import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , sortBy
                                                )
import           Control.Applicative            ( liftA2 )

type Equiv a = [[a]]

decomps :: (Num a, Eq a, Show a) => Int -> (Equiv a -> Bool) -> [a] -> Int
decomps n p ws = length $ filter p (n `colorings` ws)

-- | probably the wrong function, actually want just partitions?
colorings :: Int -> [a] -> [Equiv a]
colorings n xs =
  [ map (map fst) $ groupUnsortedOn snd cxs | cxs <- n `colors` xs ]
  where groupUnsortedOn f = groupBy ((==) `on` f) . sortBy (compare `on` f)

-- | @colors n xs@ gives the $n$-colorations of `xs`
colors :: Int -> [a] -> [[(a, Int)]]
colors _ []       = [[]]
colors n (x : xs) = liftA2 (:) (map (x, ) (std n)) (colors n xs)

choose :: [a] -> Int -> [[a]]
choose (x : xs) n | n > 0 = map (x :) (xs `choose` (n - 1)) ++ xs `choose` n
choose xs _               = [ [] | null xs ]

std :: (Num a, Enum a) => a -> [a]
std n = [0 .. n - 1]

ive :: Num a => Bool -> a
ive p = if p then 1 else 0

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
