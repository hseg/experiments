{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.Trace.O.Stolz
  ( domain
  , method
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Math.Partitions                ( expNot
                                                , weight
                                                )
import           Math.Combinat.Partitions.Integer
                                                ( Partition )
import           Math.Combinatorics             ( both
                                                , choose
                                                , std
                                                , matching
                                                )
import           Math.TypeNats                  ( reifyNats )

method :: Int -> Partition -> Int
method = const $ product . map f . expNot
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

  n `c` k = reifyNats
    n
    k
    (\(_ :: Proxy n) (_ :: Proxy k) ->
      fromIntegral $ length (choose @k $ std @n)
    )
  offsets j a = j ^ (a `quot` 2)

domain :: (Int, Partition) -> Bool
domain (n, l) = weight l <= 2 * n
