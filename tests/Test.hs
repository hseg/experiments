module Test (tests) where

import           Distribution.TestSuite.QuickCheck
import           Props.Consistent (prop_impls_consistent)


tests :: IO [Test]
tests = return
    [ testProperty "All methods consistent" prop_impls_consistent
    ]
