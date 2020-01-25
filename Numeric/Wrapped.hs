{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Wrapped
  ( WrappedApplicative(..)
  , WrappedRep(..)
  )
where
import           Numeric.Algebra                ( Abelian
                                                , Additive(..)
                                                , Algebra(..)
                                                , Group(..)
                                                , LeftModule(..)
                                                , Monoidal(..)
                                                , Multiplicative(..)
                                                , Rig(..)
                                                , RightModule(..)
                                                , Ring(..)
                                                , Semiring
                                                , Unital(..)
                                                , UnitalAlgebra(..)
                                                )
import           Numeric.Module.Representable   ( addRep
                                                , subtractRep
                                                , minusRep
                                                , sinnum1pRep
                                                , sinnumRep
                                                , zeroRep
                                                , negateRep
                                                , timesRep
                                                , oneRep
                                                , mulRep
                                                , fromNaturalRep
                                                , fromIntegerRep
                                                )
import           Data.Distributive              ( Distributive(..) )
import           Data.Functor.Rep               ( Representable(..)
                                                , distributeRep
                                                )


newtype WrappedApplicative f a = WrapApplicative (f a)
  deriving
    (Functor, Show)
  deriving newtype
    Applicative

instance (Applicative f, Additive r) => Additive (WrappedApplicative f r) where
  (+)      = addRep
  sinnum1p = sinnum1pRep
instance (Applicative f, LeftModule r s) => LeftModule r (WrappedApplicative f s) where
  r .* (WrapApplicative v) = WrapApplicative $ fmap (r .*) v
instance (Applicative f, RightModule r s) => RightModule r (WrappedApplicative f s) where
  (WrapApplicative v) *. r = WrapApplicative $ fmap (*. r) v
instance (Applicative f, Monoidal r) => Monoidal (WrappedApplicative f r) where
  zero   = zeroRep
  sinnum = sinnumRep
instance (Applicative f, Group r) => Group (WrappedApplicative f r) where
  (-)      = minusRep
  negate   = negateRep
  subtract = subtractRep
  times    = timesRep
instance (Applicative f, Abelian r) => Abelian (WrappedApplicative f r)

newtype WrappedRep f a = WrapRep (f a)
  deriving
    (Applicative, Functor, Show)
  deriving newtype
    Representable

deriving via (WrappedApplicative (WrappedRep f) r) instance (Applicative f, Additive r) => Additive           (WrappedRep f r)
deriving via (WrappedApplicative (WrappedRep f) s) instance (Applicative f, LeftModule r s) => LeftModule r   (WrappedRep f s)
deriving via (WrappedApplicative (WrappedRep f) s) instance (Applicative f, RightModule r s) => RightModule r (WrappedRep f s)
deriving via (WrappedApplicative (WrappedRep f) r) instance (Applicative f, Monoidal r) => Monoidal           (WrappedRep f r)
deriving via (WrappedApplicative (WrappedRep f) r) instance (Applicative f, Group r) => Group                 (WrappedRep f r)
instance (Applicative f, Abelian r) => Abelian (WrappedRep f r)
instance Representable f => Distributive (WrappedRep f) where
  distribute = distributeRep

type Rapp f c = (c (Rep f), Representable f, Applicative f)

instance Rapp f (Algebra r) => Multiplicative (WrappedRep f r) where
  (*) = mulRep
instance (Rapp f (UnitalAlgebra r), Unital r) => Unital (WrappedRep f r) where
  one = oneRep

instance (Rapp f (Algebra r), Semiring r) => Semiring (WrappedRep f r)

instance (Rapp f (UnitalAlgebra r), Rig r) => Rig (WrappedRep f r) where
  fromNatural = fromNaturalRep
instance (Rapp f (UnitalAlgebra r), Ring r) => Ring (WrappedRep f r) where
  fromInteger = fromIntegerRep
