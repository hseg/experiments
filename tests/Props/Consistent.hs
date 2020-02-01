{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Props.Consistent where

import           Math.Trace.O.Index             ( impls )

import           Math.Combinat.Partitions.Integer
                                                ( partitionsWithKParts )
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Data.List                      ( group )
import           Data.Either                    ( rights )
import           Control.Applicative            ( liftA2 )
import           Data.Coerce                    ( coerce )

prop_impls_consistent :: Property
prop_impls_consistent =
  forAll
      (liftA2
        (,)
        (coerce $ arbitrary @(Positive Int))
        (          arbitrary @(Positive Int, Positive Int)
        `suchThat` uncurry (<=)
        >>=        elements
        .          uncurry partitionsWithKParts
        .          coerce
        )
      )
    $ consistent (map uncurry impls)

consistent :: Eq b => [a -> Either e b] -> a -> Bool
consistent = flip (\x -> (<= 1) . length . group . rights . map ($ x))
