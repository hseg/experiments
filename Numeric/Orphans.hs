{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Numeric.Orphans where
import           Prelude                 hiding ( fromInteger
                                                , Rational
                                                )
import           Numeric.Algebra                ( Additive
                                                , Monoidal
                                                , Group
                                                , LeftModule
                                                , RightModule
                                                , Monoidal(..)
                                                , Unital(..)
                                                , UnitalAlgebra(..)
                                                , Rig(..)
                                                , Ring(..)
                                                )
import           Numeric.Field.Fraction         ( Fraction )
import           GHC.TypeNats                   ( KnownNat )
import           Data.Vector.Sized              ( Vector )
import           Numeric.Wrapped                ( WrappedApplicative(..) )

type Rational = Fraction Integer

ive :: (Unital p, Monoidal p) => Bool -> p
ive p = if p then one else zero

delta :: (Unital p, Monoidal p, Eq a) => a -> a -> p
delta x y = ive (x == y)

deriving via (WrappedApplicative (Vector n) r) instance (KnownNat n, Additive r) => Additive (Vector n r)
deriving via (WrappedApplicative (Vector n) s) instance (KnownNat n, LeftModule r s) => LeftModule r (Vector n s)
deriving via (WrappedApplicative (Vector n) s) instance (KnownNat n, RightModule r s) => RightModule r (Vector n s)
deriving via (WrappedApplicative (Vector n) r) instance (KnownNat n, Monoidal r) => Monoidal (Vector n r)
deriving via (WrappedApplicative (Vector n) r) instance (KnownNat n, Group r) => Group (Vector n r)

instance (Rig r, UnitalAlgebra r a) => Rig (a -> r) where
  fromNatural = unit . fromNatural
instance (Ring r, UnitalAlgebra r a) => Ring (a -> r) where
  fromInteger = unit . fromInteger
