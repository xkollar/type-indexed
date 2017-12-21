{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.TypeIndexed.QQ
    ( k
    )
  where

import Control.Monad (fail)
import Data.Proxy (Proxy(Proxy))
import Data.String (String)
import Data.Function (($))

import Language.Haskell.TH (Q, Exp, litT, strTyLit)
import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter), quoteExp, quotePat, quoteType, quoteDec)


-- Kinda feels that I have seen this somewhere...
k :: QuasiQuoter
k = QuasiQuoter
    { quoteExp  = mkProxy
    , quotePat  = err
    , quoteType = err
    , quoteDec  = err
    }
  where
    err _ = fail "k QQ make sense only in Expression context"

mkProxy :: String -> Q Exp
mkProxy x = [|Proxy::Proxy $(litT $ strTyLit x)|]
