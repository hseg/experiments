{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.TrigPoly where

import           Prelude                 hiding ( (+)
                                                , (-)
                                                , (*)
                                                , cos
                                                , exp
                                                , recip
                                                , negate
                                                , fromInteger
                                                , Rational
                                                )
import           Math.MonoidAlgebra             ( MonoidBasis(..) )
import           Numeric.Orphans                ( delta
                                                , ive
                                                , Rational
                                                )

import           Numeric.Algebra                ( Additive(..)
                                                , Group(..)
                                                , Unital(..)
                                                , Monoidal(..)
                                                , Division(..)
                                                , Partitionable
                                                , Multiplicative(..)
                                                )
import           Numeric.Exp                    ( Exp(..) )
import           Numeric.Field.Fraction         ( (%) )

import           Data.Finite                    ( Finite )
import           GHC.TypeNats                   ( KnownNat )
import           Data.Vector.Sized              ( Vector
                                                , generate
                                                )

type ConcreteMonomial n = Vector n Rational
type Monomial n = MonoidBasis (Exp (ConcreteMonomial n))
type Span c b = b -> c
type TrigPoly n r = r `Span` Monomial n
type F = Rational

integrate :: KnownNat n => TrigPoly n F -> F
integrate = constantTerm
 where
  constantTerm :: KnownNat n => TrigPoly n F -> F
  constantTerm = flip ($) (M (Exp zero))

exp :: ConcreteMonomial n -> Monomial n
exp = M . Exp

indet :: KnownNat n => Finite n -> Monomial n
indet i = exp $ generate (ive . (== i))

monom :: Monomial n -> TrigPoly n F
monom = delta

scale :: KnownNat n => Integer -> (TrigPoly n F -> TrigPoly n F)
scale = (*) . constant . (% 1)

-- | defined explicitly to avoid GHC getting confused by the literals
half :: F
half = 1 % 2

constant :: KnownNat n => F -> TrigPoly n F
constant r m = if m == exp zero then r else zero

fromFunc :: (ConcreteMonomial n -> F) -> TrigPoly n F
fromFunc f (M (Exp v)) = f v

-- substitute :: Multiplicative r => (Fin n -> r) -> (Monomial n -> r)
-- substitute f (M (Exp v)) = product $ imap (\i k -> (f i)^k) v

-- embed
--   :: forall m n k
--    . (KnownNat m, KnownNat k, (k + m) ~ n)
--   => TrigPoly m F
--   -> TrigPoly n F
-- embed f (M (Exp mi)) | drop mi == replicate @m 0 = f (M (Exp (drop mi)))
--                      | otherwise                 = zero

instance KnownNat n => Partitionable (Vector n Rational) where

sin², sin½², cos½², cos
  :: (KnownNat n, Division (Monomial n)) => Monomial n -> TrigPoly n F
sin² x = negate $ ((delta x - delta (recip x)) * constant half) `pow` 2
sin½² x = (constant one - cos x) * constant half -- 2sin²θ=1-cos 2θ
cos½² x = (constant one + cos x) * constant half -- 2cos²θ=1+cos 2θ
cos x = (delta x + delta (recip x)) * constant half
