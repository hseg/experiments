{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
  )
where

import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , sortBy
                                                )
import           Control.Applicative            ( liftA2 )

import           Data.Foldable                  ( toList )
import           Data.Vector.Sized              ( Vector
                                                , cons
                                                , empty
                                                , generate
                                                )
import           Data.Finite                    ( Finite )

import           Math.TypeNats                  ( cataKnownNat )
import           GHC.TypeNats                   ( KnownNat
                                                , Nat
                                                , type (<=)
                                                , type (-)
                                                )

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
colors n (x : xs) = liftA2 (:) (map (x, ) [0 .. n - 1]) (colors n xs)

choose :: forall d a t . (KnownNat d, Foldable t) => t a -> [Vector d a]
choose ts = cataKnownNat @d [empty] (step $ toList ts)
 where
  step :: forall (n :: Nat) . (KnownNat n, 1<=n) => [a] -> [Vector n a]
  step []       = []
  step (x : xs) = map (cons x) (choose @(n - 1) xs) ++ choose @n xs

std :: KnownNat n => Vector n (Finite n)
std = generate id

ive :: Num a => Bool -> a
ive p = if p then 1 else 0

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
f &&& g = \x -> (f x, g x)
-- cannot write this as @both ($x)@ since that monomorphises @($x)@

-- Might want to export some kind of lens exposing a list's length, under iso
-- std↔fromIntegral.length

matching :: (Num a, Ord a) => a -> a
matching = doubleFactorial . subtract 1

multiFactorial :: (Num a, Ord a) => a -> a -> a
multiFactorial k = product . takeWhile (>= k) . iterate (subtract k)

factorial, doubleFactorial :: (Num a, Ord a) => a -> a
factorial = multiFactorial 1
doubleFactorial = multiFactorial 2
