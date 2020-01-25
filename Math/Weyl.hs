{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Weyl
  ( weylInteg
  )
where

-- import           Numeric.GSL.Integration        ( integrateQNG )
-- import           Control.Monad                  ( replicateM )
-- import           Control.Monad.Cont             ( runCont
--                                                 , cont
--                                                 )
import           Math.Representations           ( Lie(..) )
import           Data.Vector.Sized              ( Vector )

-- weylInteg :: (Int -> Lie) -> ([Complex] -> Complex) -> (Int -> Complex)
weylInteg :: Lie d r c -> (Vector d Double -> Double) -> Double
weylInteg Lie {..} f = recip (fromIntegral order) * (int . symm $ f)
  where int = undefined
--  eps = 0.1
--  int :: ([Double] -> Double) -> Double
--  int = iter rank (\phi -> fst $ integrateQNG eps phi 0 pi)

-- iter :: Int -> ((a -> r) -> r) -> (([a] -> r) -> r)
-- iter n i = runCont $ replicateM n (cont i)

{-
import           Math.Combinatorics             ( choose )

import           Control.Applicative

-- function is expected to be variadic
-- e.g. of form `(^l) . sum . map (^k)`
expectO = ((/ 2) .) . (expectSO +@ expectSOm)
 where
  (+@) :: Num a => (b -> c -> a) -> (b -> c -> a) -> (b -> c -> a)
  (+@) = liftA2 . liftA2 $ (+)

[expectSO, expectSOm] = map (\(k, s) n f -> int n $ \hs -> s n f hs * k n hs)
                            [(kso, sso), (ksm, ssm)]
 where
  eps = 0.1
  int n = iter n (\phi -> fst $ integrateQNG eps phi 0 pi)
  conj xs = xs >>= \x -> [x, recip x]
  (?) p = (if p then id else flip) const
  sso n f = f . (even n ? id $ (1 :)) . conj
  ssm n f = f . (even n ? (1 :) $ id) . (-1 :) . conj

-- kernels for integration against SO, SOˉ
-- params: n, θᵢ
kso, ksm :: (Floating a, Integral b) => b -> [a] -> a
[kso, ksm] = map (\ks n -> (even n ?? ks) n) [(kpe, kpo), (kne, kno)]
 where
  (??) p = if p then fst else snd
  kpe, kpo, kno, kne :: (Floating a, Integral b) => b -> [a] -> a
  [kpe, kpo, kno, kne] =
    zipWith (**) (map (fromIntegral .) [const 2, (2 ^), (2 ^), (2 ^)])
      $ map (liftA2 (*) kern) [rpe, rpo, rno, rne]
   where
    f ** g = \x y -> f x * g y
    kern hs =
      abs . product $ [ 2 * cos hk - 2 * cos hj | [hk, hj] <- hs `choose` 2 ]
    [rpe, rpo, rno, rne] = map
      (\f -> product . map f)
      [const 1, \h -> sin (h / 2) ^ 2, \h -> cos (h / 2) ^ 2, \h -> sin h ^ 2]
-}
