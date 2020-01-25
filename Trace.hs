{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Trace
  ( eqns
  , stolz
  , gesh
  , table
  )
where

import           Math.Combinatorics             ( both
                                                , choose
                                                , decomps
                                                , ive
                                                , std
                                                , matching
                                                , weight
                                                )

import           Math.Combinat.Partitions.Integer
                                                ( Partition
                                                , toPartition
                                                , fromPartition
                                                )
import           Data.List                      ( group )
import           Data.Maybe                     ( listToMaybe
                                                , fromMaybe
                                                )
import           Control.Applicative            ( liftA2 )

import           Data.Proxy                     ( Proxy )
import           GHC.TypeNats                   ( SomeNat(..)
                                                , someNatVal
                                                )

expNot :: Partition -> [(Int, Int)]
expNot = map collapseOccs . group . fromPartition
  where collapseOccs xs = (length xs, fromMaybe 1 $ listToMaybe xs)

eqns :: [Int -> Partition -> Either Int Int]
stolz, gesh, table :: Int -> Partition -> Either Int Int
eqns@(stolz : gesh : table : _) = map
  (\(p, f) -> curry $ liftA2 s2p p (uncurry f))
  [ (\(n, l) -> weight l <= 2 * n, stolz')
  , (\(n, _) -> n <= 3           , gesh')
  , ((`elem` map fst known), curry $ fromMaybe 0 . flip lookup known)
  ]
 where
  s2p True  = Right
  s2p False = Left

stolz', gesh' :: Int -> Partition -> Int

stolz' = const $ product . map f . expNot
 where
  f = fromIntegral . f' . both fromIntegral
  -- f (a,2n)=#(([a]→^{≤2})
  -- f (a,2n+1)=#(([a]→²)×(ℤ/j-ℤ/j))
  -- (where X→ᵏn are the maps X→n s.t. each point has k preimages,
  -- S-T={s-t|s∈S,t∈T})
  f' (0, _) = 1
  f' (a, j)
    | odd (a * j) = 0
    | odd j = offsets j a * matching a
    | even j = sum . flip map [0, 2 .. a] $ \d ->
      a `c` (2 * d) * offsets j d * matching d
    | otherwise = error "impossible"

  n `c` k = case (someNatVal n, someNatVal k) of
    (SomeNat (_ :: Proxy n'), SomeNat (_ :: Proxy k')) ->
      fromIntegral $ length (choose @k' $ std @n')
  offsets j a = j ^ (a `quot` 2)

gesh' 1 l = ive . even $ weight l
gesh' 2 l =
  let l' = fromPartition l
  in  (ive . even $ weight l)
        *      ( decomps 2 (all ((== (weight l `quot` 2)) . sum)) l'
               + (ive (all (\(a, j) -> (a > 0) |- even j) es) * product
                   (map ((2 ^) . fst) es)
                 )
               )
        `quot` 2
 where
  es = expNot l
  p |- q = not p || q
gesh' 3 l =
  let l' = fromPartition l
  in  2
        * decomps 2 ((`ofPat` [2 `n` 0, 2 `n` 0, 2 `n` 0]) . map sum)    l'
        - decomps 3 ((`ofPat` [1 `n` (-1), 2 `n` 0, 2 `n` 1]) . map sum) l'
        - decomps 3 ((`ofPat` [1 `n` 0, 1 `n` (-1), 2 `n` 1]) . map sum) l'
 where
  n :: (Integral a, Ord a) => a -> a -> a -> Bool
  d `n` a = \x -> (x - a) >= 0 && ((x - a) `mod` d == 0)
  ofPat :: [a] -> [a -> Bool] -> Bool
  ofPat = (and .) . zipWith (flip ($))
gesh' _ _ = -1 -- an impossible value

known :: [((Int, Partition), Int)]
known =
  map (\n -> ((n, toPartition [1]), 0)) [1 .. 3]
    ++ [ ((2, toPartition [2])   , 1)
       , ((3, toPartition [2])   , 0)
       , ((2, toPartition [1, 1]), 1)
       ]
