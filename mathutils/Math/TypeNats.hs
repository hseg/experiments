{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Math.TypeNats
  ( cataKnownNat
  , axiom
  , reifyNat
  , reflectNat
  , reifyNats
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           GHC.TypeNats                   ( KnownNat
                                                , Nat
                                                , natVal
                                                , type (<=)
                                                )

import           Data.Constraint                ( Dict(..) )
import           Unsafe.Coerce                  ( unsafeCoerce )

cataKnownNat
  :: forall (n :: Nat) r . KnownNat n => ((n ~ 0) => r) -> ((1 <= n) => r) -> r
cataKnownNat z s = case natVal (Proxy @n) of
  0 -> unsafeRefl @(n~0) z
  _ -> unsafeRefl @(1<=n) s

unsafeRefl :: forall c r . (c => r) -> r
unsafeRefl k = case unsafeCoerce (Dict :: Dict (a ~ a)) :: Dict c of
  Dict -> k

reifyNats
  :: Integer
  -> Integer
  -> (  forall (n :: Nat) (m :: Nat)
      . (KnownNat n, KnownNat m)
     => Proxy n
     -> Proxy m
     -> r
     )
  -> r
reifyNats n m k = reifyNat n (\pn -> reifyNat m (\pm -> k pn pm))
{- HLINT ignore reifyNats "Avoid lambda" -}
-- without lambda, unification doesn't happen

reflectNat :: forall n a. (KnownNat n, Num a) => a
reflectNat = fromIntegral . natVal $ Proxy @n
