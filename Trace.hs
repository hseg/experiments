module Trace
  ( eqns
  , stolz
  , table
  )
where

import           Math.Combinatorics             ( both
                                                , choose
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

expNot :: Partition -> [(Int, Int)]
expNot = map collapseOccs . group . fromPartition
  where collapseOccs xs = (length xs, fromMaybe 1 $ listToMaybe xs)

eqns :: [Int -> Partition -> Either Int Int]
stolz, table :: Int -> Partition -> Either Int Int
eqns@(stolz : table : _) = map
  (\(p, f) -> curry $ liftA2 s2p p (uncurry f))
  [ (\(n, l) -> weight l <= 2 * n, stolz')
  , ((`elem` map fst known), curry $ fromMaybe 0 . flip lookup known)
  ]
 where
  s2p True  = Right
  s2p False = Left

stolz' :: Int -> Partition -> Int

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

  n `c` k = fromIntegral $ length (std n `choose` k)
  offsets j a = j ^ (a `quot` 2)

known :: [((Int, Partition), Int)]
known =
  map (\n -> ((n, toPartition [1]), 0)) [1 .. 3]
    ++ [ ((2, toPartition [2])   , 1)
       , ((3, toPartition [2])   , 0)
       , ((2, toPartition [1, 1]), 1)
       ]
