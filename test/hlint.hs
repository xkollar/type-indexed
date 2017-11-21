{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Monad (unless)
import Data.List (null)
import System.Exit (exitFailure)
import System.IO (IO, putStrLn)

import Language.Haskell.HLint (hlint)


main :: IO ()
main = do
    putStrLn ""
    hints <- hlint ["."]
    unless (null hints) exitFailure
