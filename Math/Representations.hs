{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RebindableSyntax #-}

module Math.Representations
  ( Lie(..)
  , sl
  , so
  , so'
  , sp
  , a
  , b
  , b'
  , c
  , d
  , d'
  )
where

-- {{{ Imports
import           Prelude                 hiding ( sin
                                                , cos
                                                , exp
                                                , concatMap
                                                , head
                                                , last
                                                , (++)
                                                , (/)
                                                , (*)
                                                , (^)
                                                , (-)
--                                                , fromInteger
                                                , negate
                                                , subtract
                                                , product
                                                , sum
                                                )
import           Numeric.Algebra                ( pow
                                                , (*)
                                                , zero
                                                , one
--                                                , fromInteger
                                                , negate
                                                , product
                                                , sum
                                                , Group(..)
                                                , Natural
                                                )
import           Math.Combinatorics             ( choose
                                                , factorial
                                                , std
                                                )
import           Numeric.Orphans                ( ive )
import           Math.TypeNats                  ( axiom )
import           Math.TrigPoly                  ( ConcreteMonomial
                                                , F
                                                , TrigPoly
                                                , Monomial
                                                , scale
                                                , indet
                                                , fromFunc
                                                , sin², sin½², cos½², cos
                                                , half
                                                , exp
                                                )

import           Data.Vector.Sized              ( Vector
                                                , index
                                                , head
                                                , last
                                                , cons
                                                , fromTuple
                                                , generate
                                                , concatMap
                                                , (++)
                                                , singleton
                                                , empty
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Finite                    ( Finite )
import           GHC.TypeNats                   ( KnownNat, Nat, type (+), type (*), natVal )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Type.Equality             ( gcastWith )
import           Data.Reflection                ( reifyNat )
-- }}}

-- {{{ Defns
-- | $d=\dim 𝔤$, $r=\rank V$
data Lie (d::Nat) (r::Nat) c = Lie
 {order :: Natural -- ^ of weyl group
 ,delta2 :: TrigPoly r c
 ,symm :: (ConcreteMonomial d -> c) -> TrigPoly r c
 }

type WeylIntegration c
  = (forall d r . (KnownNat d, KnownNat r) => Lie d r c -> c) -> c
-- }}}

-- {{{ Utils
-- | @(f1,f2) = fromIso f to fr@ satisfy @f1 x = f x (to x) = f (fr y) y = f2 y@
-- where @y = to x@ ⇔ @x = fr y@
-- fromIso
--   :: (a -> b -> c)
--   -> (a -> b)
--   -> (b -> a)
--   -> (a -> c, b -> c)
-- fromIso h f f' = (uncurry h . (id &&& f), uncurry h . (f' &&& id))
-- Useless with rank-n types -- haskell doesn't support impredicative
-- polymorphism

uncurryV :: (a -> a -> b) -> Vector 2 a -> b
uncurryV f xs = f (head xs) (last xs)

cosD :: forall n . KnownNat n => Vector 2 (Monomial n) -> TrigPoly n F
cosD = scale 2 . uncurryV (-) . fmap cos

delta :: forall r . KnownNat r => Vector 2 (Finite r) -> Monomial r
delta is = exp . generate @r $ \t ->
  fromMaybe zero $ lookup t [(i, half), (j, negate half)]
  where [i, j] = [index is 0, index is 1]

nSₙ, n2ᵏ :: Natural -> Natural
nSₙ = factorial
n2ᵏ k = 2 `pow` k
-- }}}

-- {{{ SL/A
-- |Type: Aₗ
--  Group: SL(l+1)
--  Defining condition: det=1
--  Maximal torus: diagonal matrices
--  Weyl group: Sₙ
a, sl :: Int -> WeylIntegration F
a r = mkSL (r + 1) r
sl d = mkSL d (d - 1)

mkSL :: Int -> Int -> WeylIntegration F
mkSL (fromIntegral -> d) (fromIntegral -> r) k = reifyNat
  d
  (\(_ :: Proxy d) -> reifyNat
    r
    (\(_ :: Proxy r) -> k @d @r (gcastWith (axiom @Nat @d @(r+1)) (slA @d @r)))
  )
 where
  slA :: forall d r . (d ~ (r+1), KnownNat d, KnownNat r) => Lie d r F
  slA = Lie { .. }
   where
    dim   = natVal $ Proxy @d
    order = nSₙ dim
    symm f = fromFunc $ \hs -> f (negate (sum hs) `cons` hs)
    delta2 =
      product
        . fmap (scale 4 . sin² . delta)
        . filter (uncurryV (<))
        $ choose @2 (std @r)
    -- Note: cf Goodman&Wallach, absorbed 4^{2{l\choose 2}} coefficient in product
    --       Took ordered pairs since I can't cleanly capture the swapping
    --       symmetry without having to implement sin
-- }}}

-- {{{ SO/D/B
-- {{{ Docs
-- |Type: Dₗ
--  Group: SO(2l)
--  Defining condition: det=1, fixed by (·^T)ˉ¹
--  Maximal torus: matrices decomposing into ((cos, -sin),(sin,cos)
--  Weyl group: 2ˡ≀Sₗ/2 (even sign changes only)

-- |Type: Bₗ
--  Group: SO(2l+1)
--  Defining condition: det=1, fixed by (·^T)ˉ¹
--  Maximal torus: matrices decomposing into ((cos, -sin),(sin,cos), 1
--  Weyl group: 2ˡ≀Sₗ

-- |Type: Dₗ'
--  Group: SOˉ(2l+2)
--  Defining condition: det=1, fixed by -(·^T)ˉ¹
--  Maximal torus: matrices decomposing into ((cos, -sin),(sin,cos), 1, -1
--  Weyl group: 2ˡ≀Sₗ/2 (even sign changes only)

-- |Type: Bₗ'
--  Group: SOˉ(2l+1)
--  Defining condition: det=1, fixed by -(·^T)ˉ¹
--  Maximal torus: matrices decomposing into ((cos, -sin),(sin,cos), -1
--  Weyl group: 2ˡ≀Sₗ
--  }}}

d, b, so, d', b', so' :: Int -> WeylIntegration F
d r = mkSO @ 'E @ 'P (soR2D @ 'E @ 'P r) r
b r = mkSO @ 'O @ 'P (soR2D @ 'O @ 'P r) r
so d | even d = mkSO @ 'E @ 'P d (soD2R @ 'E @ 'P d)
     | odd d  = mkSO @ 'O @ 'P d (soD2R @ 'O @ 'P d)
d' r = mkSO @ 'E @ 'N (soR2D @ 'E @ 'N r) r
b' r = mkSO @ 'O @ 'N (soR2D @ 'O @ 'N r) r
so' d k | d == 0 = error "SOˉ(0) doesn't exist"
        | even d = mkSO @ 'E @ 'N d (soD2R @ 'E @ 'N d) k
        | odd d  = mkSO @ 'O @ 'N d (soD2R @ 'O @ 'N d) k

-- {{{ Special-casing
soR2D
  :: forall (p :: Parity) (s :: Sign)
   . KnownNat (NumTrivEigens p s)
  => Int
  -> Int
soR2D r = r * 2 + fromIntegral (natVal (Proxy @(NumTrivEigens p s)))
soD2R
  :: forall (p :: Parity) (s :: Sign)
   . KnownNat (NumTrivEigens p s)
  => Int
  -> Int
soD2R d = (d - fromIntegral (natVal (Proxy @(NumTrivEigens p s)))) `div` 2

data Parity = E | O
data Sign = P | N

class SO (p::Parity) (s::Sign) where
    type NumTrivEigens p s :: Nat
    trivialEigens :: Vector (NumTrivEigens p s) F
    delta2lin :: forall n. KnownNat n => Monomial n -> TrigPoly n F

instance SO 'E 'P where
  type NumTrivEigens 'E 'P = 0
  trivialEigens = empty
  delta2lin     = const one

instance SO 'O 'P where
  type NumTrivEigens 'O 'P = 1
  trivialEigens = singleton one
  delta2lin     = scale 4 . sin½²

instance SO 'O 'N where
  type NumTrivEigens 'O 'N = 1
  trivialEigens = singleton (negate one)
  delta2lin     = scale 4 . cos½²

instance SO 'E 'N where
  type NumTrivEigens 'E 'N = 2
  trivialEigens = fromTuple (one, negate one)
  delta2lin     = scale 4 . sin²
-- }}}

mkSO
  :: forall (p :: Parity) (s :: Sign)
   . SO p s
  => Int
  -> Int
  -> WeylIntegration F
mkSO (fromIntegral -> d) (fromIntegral -> r) k = reifyNat
  d
  (\(_ :: Proxy d) -> reifyNat
    r
    (\(_ :: Proxy r) ->
      k @d @r
        (gcastWith (axiom @Nat @d @(NumTrivEigens p s+2*r)) (soBD @p @s @d @r))
    )
  )
 where
  soBD
    :: forall (p :: Parity) (s :: Sign) d r t
     . (d ~ (t+2*r), t ~ NumTrivEigens p s, SO p s, KnownNat d, KnownNat r)
    => Lie d r F
  soBD = Lie { .. }
   where
    rank  = natVal $ Proxy @r
    order = n2ᵏ rank * nSₙ rank
    symm f = fromFunc $ f . mkEigen (trivialEigens @p @s)
     where
      mkEigen :: Group a => Vector n a -> Vector m a -> Vector (n+2*m) a
      mkEigen fs hs = fs ++ concatMap (\h -> fromTuple (h, negate h)) hs
    delta2 = product (fmap (cosD . fmap indet) (choose @2 (std @r)))
      * product (fmap (delta2lin @p @s) (generate indet))
-- }}}

-- {{{ Sp/C
-- |Type: Cₗ
--  Group: Sp(2l)
--  Defining condition: det=1, fixed by -·^T
--  Maximal torus: diagonal matrices decomposing into [x,xˉ]
--  Weyl group: 2ˡ≀Sₗ
c, sp :: Int -> WeylIntegration F
c r = mkSp (2 * r) r
sp d k | even d    = mkSp d (d `div` 2) k
       | otherwise = error "Sp(2k+1) is undefined"

mkSp :: Int -> Int -> WeylIntegration F
mkSp (fromIntegral -> d) (fromIntegral -> r) k = reifyNat
  d
  (\(_ :: Proxy d) -> reifyNat
    r
    (\(_ :: Proxy r) -> k @d @r (gcastWith (axiom @Nat @d @(r*2)) (spC @d @r)))
  )
 where
  spC :: forall d r . (d ~ (r*2), KnownNat d, KnownNat r) => Lie d r F
  spC = Lie { .. }
   where
    dim   = natVal $ Proxy @d
    rank  = natVal $ Proxy @r
    order = n2ᵏ rank * nSₙ rank
    symm f = fromFunc $ f . concatMap (\h -> fromTuple (h, negate h))
    delta2 =
      ive (dim == zero)
        * product (fmap (cosD . fmap indet) (choose @2 $ std @r))
        * product (fmap (scale 4 . sin²) (generate indet))
    -- Note: Meckes defines this as ∏_{i<j}(2cos θᵢ-2cos θⱼ)²∏sin²θᵢ and elides
    --           a multiplication by 4ˡ
    --       Goodman&Wallach define this as 4^{l²}∏_{i<j}sin²∆ᵢⱼsin²∇ᵢⱼ∏sin²θᵢ
    --           where Δᵢⱼ=½(θᵢ-θⱼ), ∇ᵢⱼ=½(θᵢ+θⱼ)
    --       We define this as ∏ᵢⱼ2|cos θᵢ-cos θⱼ|∏4sin²θᵢ
    --       These are equivalent:
    --       4²sin²∆sin²∇=(2cos-2cos)²=4(cos-cos)²=(2|cos-cos|)(2|cos-cos|)
    --       and l²=2(l\choose 2)+l
-- }}}
