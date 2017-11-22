{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
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
       (Has, M, T, cons, empty, get, update)


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
example1 = cons (Proxy @"a") () $ cons (Proxy @"b") 1 empty

example2 :: M '[T "key2" Bool, T "key1" Int]
example2 = cons (Proxy @"key2") True $ cons (Proxy @"key1") 1 empty

test :: (Has "key1" Int s, Has "key2" Bool s) => M s -> String
test m = show (get (Proxy @"key1") m) <> show (get (Proxy @"key2") m)
