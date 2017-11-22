{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-} --
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-} --
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} --
module Data.TypeIndexed.HeteroMap where

import Data.Bool (Bool(True), (&&))
import Data.Eq (Eq, (==))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Show (Show, showsPrec)


data X (l :: [k1]) (t :: [k2]) where
    E :: X '[] '[]
    C :: !a -> !(X ls ts) -> X (l ': ls) (a ': ts)

instance Eq (X '[] '[]) where
    E == E = True
    {-# INLINE (==) #-}

instance (Eq (X ls ts), Eq t) => Eq (X (l ': ls) (t ': ts)) where
    C v1 s1 == C v2 s2 = v1 == v2 && s1 == s2
    {-# INLINE (==) #-}

instance Show (X '[] '[]) where
    showsPrec _ E (',':s@(']':_)) = s
    showsPrec _ E s = "[]" <> s

instance (KnownSymbol l, Show (X ls ts), Show a) => Show (X (l ': ls) (a ': ts)) where
    showsPrec n (C v r) s@(',':']':_) =
        ',' : showsPrec 11 (symbolVal (Proxy @l),v) (showsPrec n r s)
    showsPrec n (C v r) s =
        '[' : showsPrec 11 (symbolVal (Proxy @l),v) (showsPrec n r (',':']':s))

empty :: X '[] '[]
empty = E
{-# INLINE empty #-}

cons :: proxy l -> a -> X ls ts -> X (l : ls) (a : ts)
cons _ = C
{-# INLINE cons #-}

class Has (l :: k1) (t :: *) (ls :: [k1]) (ts :: [k2]) | l ls ts -> t where
    get :: proxy l -> X ls ts -> t
    update :: proxy l -> t -> X ls ts -> X ls ts

instance {-# OVERLAPPABLE #-} Has l t ls ts => Has l t (l' ': ls) (t' ': ts) where
    get p (C _ s) = get p s
    {-# INLINE get #-}
    update p v (C a s) = C a (update p v s)
    {-# INLINE update #-}

instance Has l t (l ': ls) (t ': ts) where
    get _ (C x _) = x
    {-# INLINE get #-}
    update _ v (C _ s) = C v s
    {-# INLINE update #-}

-- example = get (Proxy @"a") (C () $ C 1 E :: X '["a", "b"] '[(), Int]) :: ()
-- get (Proxy @"lol") . update (Proxy @"lol") 5 . cons (Proxy @"lol") (1::Int) $ cons (Proxy @"test") () empty

-- test :: (Has "test1" Int ls ts, Has "test2" Bool ls ts) => X ls ts -> String
-- test m = show (get (Proxy @"test1") m) <> show (get (Proxy @"test2") m)
