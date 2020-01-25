import           Control.Applicative            ( liftA2 )

import           Math.Combinat.Partitions.Set
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Data

(∨) = min
(f .: g) x y = f $ g x y

-- Partitions [l]→[n] factoring through a 2-fibration [2l]↠[l]
isTwoPartition = all even . map length . fromSetPartition

-- l≤n: n

twoPartitions n l =
  filter isTwoPartition . setPartitionsWithKParts (n ∨ (l - n))

-- Constraint: lists must be finite (forcing `length` on both)
relationToMatrix :: (r -> c -> Bool) -> [r] -> [c] -> Matrix R
relationToMatrix p rs cs = (length rs >< length cs) $ liftA2 ((↯) .: p) rs cs
  where (↯) b = if b then 1 else 0
