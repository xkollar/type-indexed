{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Data.TypeIndexed.HomoMap.Internal
    ( M(..)
    , empty
    , cons
    , head
    , tail
    , Member(..)
    , Tags(..)
    , tagsOfMap
    , collect
    )
  where

import Prelude (error)

import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Functor (Functor)
import Data.Function ((.), ($))
import qualified Data.List as List
import Data.Proxy (Proxy(Proxy))
import Data.String (String)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Show (Show)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


newtype M (ts :: [k]) a = M [a]
    deriving (Eq, Show, Generic, Functor)

empty :: M '[] a
empty = M []
{-# INLINE empty #-}

cons :: proxy t -> a -> M ts a -> M (t ': ts) a
cons _ x (M s) = M (x:s)
{-# INLINE cons #-}

head :: M (t:ts) a -> a
head (M (x:_)) = x
head (M _) = error "head: impossible happened"
{-# INLINE head #-}

tail :: M (t:ts) a -> M ts a
tail (M (_:s)) = M s
tail (M _) = error "tail: impossible happened"
{-# INLINE tail #-}

class Member (t :: k) (s :: [k]) where
    get :: proxy t -> M s a -> a
    update :: proxy t -> a -> M s a -> M s a

instance Member t (t ': s) where
    get _ (M (x:_)) = x
    get _ _ = error "get: impossible happened"
    {-# INLINE get #-}
    update _ v (M (_:s)) = M (v:s)
    update _ _ (M _) = error "update: impossible happened"
    {-# INLINE update #-}

instance {-# OVERLAPPABLE #-} Member t s => Member t (t' ': s) where
    get p = get p . tail
    {-# INLINE get #-}
    update p v s = cons Proxy (head s) (update p v (tail s))
    {-# INLINE update #-}

class Tags (ts :: [k]) where
    tags :: proxy ts -> [String]

instance Tags '[] where
    tags _ = []
    {-# INLINE tags #-}

instance (KnownSymbol t, Tags s) => Tags (t ': s) where
    tags _ = symbolVal (Proxy @t) : tags (Proxy @s)
    {-# INLINE tags #-}

tagsOfMap :: forall ts a. Tags ts => M ts a -> [String]
tagsOfMap _ = tags (Proxy @ts)
{-# INLINE tagsOfMap #-}

collect :: Tags ts => proxy ts -> Map String a -> Either [String] (M ts a)
collect p m = case ts List.\\ Map.keys m of
    [] -> Right . M $ List.map (m Map.!) ts
    s -> Left s
  where
    ts = tags p
