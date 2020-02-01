module Math.Partitions
  ( expNot
  , weight
  )
where

import           Math.Combinat.Partitions.Integer
                                                ( Partition
                                                , fromPartition
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.List                      ( group )

expNot :: Partition -> [(Int, Int)]
expNot = map collapseOccs . group . fromPartition
  where collapseOccs xs = (length xs, fromMaybe 1 $ listToMaybe xs)

weight :: Partition -> Int
weight = sum . fromPartition
