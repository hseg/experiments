{-# LANGUAGE GADTs, ConstraintKinds, ScopedTypeVariables #-}

module Tensor where

import           Data.List                      ( group
                                                , sort
                                                )

type NatLike a = (Num a, Enum a, Ord a)

focs :: forall  a . NatLike a => a -> Int -> [[a]]
focs n = go 1
 where
  go :: NatLike a => a -> Int -> [[a]]
  go _ 0 = [[]]
  go m l = [1 .. m] >>= \h -> map (h :) (go (proj m n (succ h)) (pred l))
  proj l u = max l . min u

occs = map length . group . sort
spanners n = filter (all even . occs) . focs n . (2 *)
standard n = filter (all (== 2) . occs) . focs n $ (2 *) n

-- ghci

pp :: Show a => [[a]] -> IO ()
pp = putStrLn . unlines . map show
probe f = pp . take 50 . f
std = standard
tst n = spanners (n - 1) n
