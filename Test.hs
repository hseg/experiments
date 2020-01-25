{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test where

import           Trace                          ( eqns )

import           Math.Combinat.Partitions.Integer
                                                ( partitionsWithKParts )
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Data.List                      ( group )
import           Data.Either                    ( rights )
import           Control.Applicative            ( liftA2 )
import           Data.Coerce                    ( coerce )

prop_eqns_consistent :: Property
prop_eqns_consistent =
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
    $ consistent (map uncurry eqns)

consistent :: Eq b => [a -> Either e b] -> a -> Bool
consistent = flip (\x -> (<= 1) . length . group . rights . map ($ x))
