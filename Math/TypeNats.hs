{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Math.TypeNats where

import           Data.Proxy                     ( Proxy(..) )
import           GHC.TypeNats                   ( KnownNat
                                                , Nat
                                                , natVal
                                                , type (<=)
                                                )
import           Data.Type.Equality             ( (:~:)(..) )
import           Data.Constraint                ( Dict(..) )
import           Unsafe.Coerce                  ( unsafeCoerce )

cataKnownNat
  :: forall (n :: Nat) r . KnownNat n => ((n ~ 0) => r) -> ((1 <= n) => r) -> r
cataKnownNat z s = case natVal (Proxy @n) of
  0 -> unsafeRefl @(n~0) z
  _ -> unsafeRefl @(1<=n) s

axiom :: forall k (l :: k) (r :: k) . (l :~: r)
axiom = unsafeCoerce Refl

unsafeRefl :: forall c r . (c => r) -> r
unsafeRefl k = case unsafeCoerce (Dict :: Dict (a ~ a)) :: Dict c of
  Dict -> k
