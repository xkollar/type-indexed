{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.IO (IO)

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Lib (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Tests.Lib.tests
    ]
