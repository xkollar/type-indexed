{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.Lib (tests) where

import Data.Functor ((<$>))
import Data.Function (($), (.))

import Language.Haskell.TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Lib


tests :: TestTree
tests = testGroup $(LitE . StringL . loc_module <$> location)
    [ testCase "Trivial"
        $ Lib.value @?= ()
    ]
