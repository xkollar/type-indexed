{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Test.Data.TypeIndexed.HeteroMap (tests) where

import Prelude (succ)

import Data.Bool (Bool(True))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.String (String)
import Text.Show (show)

import Language.Haskell.TH
       (Exp(LitE), Lit(StringL), loc_module, location)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Data.TypeIndexed.HeteroMap
       (Has, M, T, empty, get, insert, update)
import Data.TypeIndexed.QQ (k)


tests :: TestTree
tests = testGroup $(LitE . StringL . loc_module <$> location)
    [ testCase "Compiles 1"
        $ get (Proxy @"a") example1 @?= ()
    , testCase "Update"
        $ get (Proxy @"b") (update (Proxy @"b") succ example1) @?= 2
    , testCase "Access combined"
        $ test example2 @?= "1True"
    , testCase "Show"
        $ show example1 @?= "[(\"a\",()),(\"b\",1)]"
    , testCase "Show in list"
        $ show [example1] @?= "[[(\"a\",()),(\"b\",1)]]"
    , testCase "Show in tuple"
        $ show (example1,()) @?= "([(\"a\",()),(\"b\",1)],())"
    ]

example1 :: M '[T "a" (),T "b" Int]
example1 = insert [k|a|] () $ insert [k|b|] 1 empty

example2 :: M '[T "key2" Bool, T "key1" Int]
example2 = insert [k|key2|] True $ insert [k|key1|] 1 empty

test :: (Has "key1" Int s, Has "key2" Bool s) => M s -> String
test m = show (get [k|key1|] m) <> show (get [k|key2|] m)
