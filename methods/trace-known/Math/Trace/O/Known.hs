module Math.Trace.O.Known
  ( domain
  , method
  )
where

import           Math.Combinat.Partitions.Integer
                                                ( Partition, toPartition )
import           Data.Maybe                     ( fromMaybe )

method :: Int -> Partition -> Int
method = curry $ fromMaybe 0 . flip lookup oDB

domain :: (Int, Partition) -> Bool
domain = (`elem` map fst oDB)

oDB :: [((Int, Partition), Int)]
oDB =
  map (\n -> ((n, toPartition [1]), 0)) [1 .. 3]
    ++ [ ((2, toPartition [2])   , 1)
       , ((3, toPartition [2])   , 0)
       , ((2, toPartition [1, 1]), 1)
       ]
