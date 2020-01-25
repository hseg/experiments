{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.MonoidAlgebra where

import           Prelude                 hiding ( sum
                                                , (*)
                                                )
import           Numeric.Algebra                ( Algebra(..)
                                                , Division
                                                , Factorable(..)
                                                , Multiplicative(..)
                                                , Unital(..)
                                                , UnitalAlgebra(..)
                                                , Rig(..)
                                                , sum
                                                )
import           Numeric.Orphans                ( ive )

newtype MonoidBasis a = M a deriving (Show, Eq)
deriving instance Multiplicative a => Multiplicative (MonoidBasis a)
deriving instance Division a => Division (MonoidBasis a)
deriving instance Unital a => Unital (MonoidBasis a)
deriving instance Factorable a => Factorable (MonoidBasis a)

instance (Factorable a, Rig r) => Algebra r (MonoidBasis a) where
  mult f = sum . factorWith f
instance (Unital a, Eq a, Factorable a, Rig r) => UnitalAlgebra r (MonoidBasis a) where
  unit x = (x *) . ive . (== one)
