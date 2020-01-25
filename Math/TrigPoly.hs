{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Math.TrigPoly where

import           Prelude                 hiding ( (+)
                                                , (-)
                                                , (/)
                                                , (*)
                                                , negate
                                                , recip
                                                , fromInteger
                                                , drop
                                                , replicate
                                                , zipWith
                                                , Rational
                                                , lookup
                                                )
import           Math.MonoidAlgebra
import           Numeric.Orphans

import           Numeric.Algebra
import           Numeric.Algebra.Complex
import           Numeric.Exp
import           Numeric.Field.Class            ( )
import           GHC.TypeNats
import           Data.Vector.Sized
import           Data.Map                       ( Map
                                                , fromList
                                                , lookup
                                                , singleton
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Foldable

type ConcreteMonomial n = Vector n Rational
type Monomial n = MonoidBasis (Exp (ConcreteMonomial n))
type TrigPoly n r = Map (Monomial n) r

instance Ord (Monomial n) where
  (M (Exp u)) `compare` (M (Exp v)) = fold $ zipWith compare u v

monom :: Unital r => Monomial n -> TrigPoly n r
monom = flip Data.Map.singleton one

fromCoeffs :: [(ConcreteMonomial n, r)] -> TrigPoly n r
fromCoeffs = Data.Map.fromList . fmap (\(m, c) -> (M (Exp m), c))

-- Not yet ported, would want to do something along the lines of zero-padding
-- all the keys in f
-- embed
--   :: forall m n k r
--    . (KnownNat m, KnownNat k, (k + m) ~ n, Monoidal r)
--   => TrigPoly m r
--   -> TrigPoly n r
-- embed f (M (Exp mi)) | drop mi == replicate @m 0 = f (M (Exp (drop mi)))
--                      | otherwise                 = zero

integrate :: (KnownNat n, Monoidal r) => TrigPoly n r -> r
integrate = constantTerm
 where
  constantTerm :: (KnownNat n, Monoidal r) => TrigPoly n r -> r
  constantTerm = fromMaybe zero . lookup (M (Exp zero))

exp :: ConcreteMonomial n -> Monomial n
exp = M . Exp

class (Commutative r, DivisionRing r, Distinguished r, Complicated r) => QiExt r where
instance (Commutative r, InvolutiveSemiring r, DivisionRing r, TriviallyInvolutive r) => QiExt (Complex r) where

-- sin @(Monomial n) :: Monomial n -> TrigPoly n r
sin, cos :: forall a r . (Division a, Ord a, QiExt r) => a -> Map a r
cos x = Data.Map.fromList [(x, recip @r (2 * e)), (recip x, recip @r (2 * e))]
sin x = Data.Map.fromList [(x, recip @r (2 * i)), (recip x, -recip @r (2 * i))]

-- TODO:
-- *   Need `choose` to continue working on `Vect n a`
--
--     *   Can convert with minimal breakage, as long as we expose
--         `std::∑_{l:Int} Vect l a` and
-- ✓   Need to implement `sin, cos :: Linear n -> TrigPoly r n`
--     *   May want to precompose `sin²½·,cos²½·`
-- ✓   `Linear` ∈ ℚ-Mod (want to be able to take sin ½(θ+φ)
-- *   Need to reflect `Lie.dim, Lie.rank` into lengths of vectors of eigenvals
--     *   NOTE: In fact, want to write `Gn=Rl -Lie→ stats(n,l)`.
--         GADT with the relevant equalities?
--         (e.g. data GR n l where G::(l~f n)=> n -> GR n l…)
-- ✓   For integration, want `getConstant`.
--     *   Preferably, abstract this detail -- general Weyl integration should
--         not know about how the measure is defined
