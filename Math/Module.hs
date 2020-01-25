module Module where

import           Numeric.Algebra
import           Data.Map                       ( Map
                                                , fromList
                                                , lookup
                                                , singleton
                                                )

instance LeftModule r m => LeftModule r (e `Map` m) where
  m .* f = fmap (m .*) f

instance Monoidal r => Monoidal (e `Map` r)
  zero = empty
  sinnum n = fmap (sinnum n)
  -- sumWith f xs e = sumWith (`f` e) xs
instance Algebra r a => Multiplicative (a `Map` r)
  -- f * g = mult $ \a b -> f a * g b
instance RightModule r m => RightModule r (e `Map` m)
  f *. m = fmap (*. m) f
instance Algebra r a => Semiring (a `Map` r)
instance Abelian r => Abelian (e `Map` r)
instance Additive r => Additive (b `Map` r)
  (+) = unionWith (+)
  sinnum1p n = fmap (sinnum1p n)
  -- sumWith1 f xs e = sumWith1 (`f` e) xs
instance Idempotent r => Idempotent (e `Map` r)
instance Group r => Group (e `Map` r)
  (-) = unionWith (-)
  negate = fmap negate
  subtract = unionWith subtract
  times n = fmap (times n)
instance CommutativeAlgebra r a => Commutative (a `Map` r)
instance (Unital r, DivisionAlgebra r a) =>
                Division (a `Map` r)
  -- Defined in ‘Numeric.Algebra.Division’
instance (TriviallyInvolutive r,
                 TriviallyInvolutiveAlgebra r a) =>
                TriviallyInvolutive (a `Map` r)
instance CommutativeAlgebra r m =>
                CocommutativeCoalgebra r (m `Map` r)

-- instance (Unital r, UnitalAlgebra r a) => Unital (a `Map` r)
-- instance (Unital r, UnitalAlgebra r m) => CounitalCoalgebra r (m `Map` r)
-- instance Algebra r m => Coalgebra r (m `Map` r)
-- instance InvolutiveAlgebra r h => InvolutiveMultiplication (h `Map` r)
