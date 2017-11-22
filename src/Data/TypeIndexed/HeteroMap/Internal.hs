{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} --
module Data.TypeIndexed.HeteroMap.Internal where

import Data.Bool (Bool(True), (&&))
import Data.Eq (Eq, (==))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Show (Show, showsPrec)

-- | Type level tuple (could not use (,) as it expects its params to be of
-- kind '*').
data T (a :: k1) (b :: k2)

data M (t::[*]) where
    E :: M '[]
    C :: !a -> !(M s) -> M (T l a : s)

instance Eq (M '[]) where
    E == E = True
    {-# INLINE (==) #-}

instance (Eq (M s), Eq t) => Eq (M (T l t : s)) where
    C v1 s1 == C v2 s2 = v1 == v2 && s1 == s2
    {-# INLINE (==) #-}

instance Show (M '[]) where
    showsPrec _ E (',':s@(']':_)) = s
    showsPrec _ E s = "[]" <> s
    {-# INLINE showsPrec #-}

instance (KnownSymbol l, Show (M s), Show a) => Show (M (T l a : s)) where
    showsPrec n (C v r) s@(',':']':_) =
        ',' : showsPrec 11 (symbolVal (Proxy @l),v) (showsPrec n r s)
    showsPrec n (C v r) s =
        '[' : showsPrec 11 (symbolVal (Proxy @l),v) (showsPrec n r (',':']':s))
    {-# INLINE showsPrec #-}

empty :: M '[]
empty = E
{-# INLINE empty #-}

cons :: proxy l -> a -> M s -> M (T l a : s)
cons _ = C
{-# INLINE cons #-}

class Has (l :: k1) (t :: *) (s :: [*]) | l s -> t where
    get :: proxy l -> M s -> t
    update :: proxy l -> (t -> t) -> M s -> M s

instance {-# OVERLAPPABLE #-} Has l t s => Has l t (T l' t' ': s) where
    get p (C _ s) = get p s
    {-# INLINE get #-}
    update p f (C a s) = C a (update p f s)
    {-# INLINE update #-}

instance Has l t (T l t ': s) where
    get _ (C x _) = x
    {-# INLINE get #-}
    update _ f (C v s) = C (f v) s
    {-# INLINE update #-}
