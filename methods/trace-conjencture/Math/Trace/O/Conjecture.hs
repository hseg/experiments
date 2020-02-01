module Math.Trace.O.Conjecture
  ( domain
  , method
  )
where

import           Math.Combinatorics             ( decomps
                                                , ive
                                                )
import           Math.Partitions                ( expNot
                                                , weight
                                                )
import           Math.Combinat.Partitions.Integer
                                                ( Partition, fromPartition )

method :: Int -> Partition -> Int
method 1 l = ive . even $ weight l
method 2 l =
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
method 3 l =
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
method _ _ = -1 -- an impossible value

domain :: (Int, Partition) -> Bool
domain (n, _) = n <= 3
