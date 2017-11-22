{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.IO (IO)

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Data.TypeIndexed.HeteroMap (tests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Test.Data.TypeIndexed.HeteroMap.tests
    ]
