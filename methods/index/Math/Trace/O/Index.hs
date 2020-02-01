module Math.Trace.O.Index
  ( impls
  )
where

import           Control.Applicative            ( liftA2 )

import           Math.Combinat.Partitions.Integer
                                                ( Partition )

import qualified Math.Trace.O.Stolz            as Stolz
                                                ( method
                                                , domain
                                                )
import qualified Math.Trace.O.Conjecture       as Conjecture
                                                ( method
                                                , domain
                                                )
import qualified Math.Trace.O.Known            as Known
                                                ( method
                                                , domain
                                                )

impls :: [Int -> Partition -> Either Int Int]
impls = map
  (\(p, f) -> curry $ liftA2 s2p p (uncurry f))
  [ (Stolz.domain, Stolz.method)
  , (Conjecture.domain , Conjecture.method)
  , (Known.domain, Known.method)
  ]
 where
  s2p True  = Right
  s2p False = Left
