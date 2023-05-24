{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Streaming.ByteString.Internal.Types where

import Prelude.Linear ()

import Control.Functor.Linear qualified as Control
import Data.ByteString (ByteString)
import Data.Functor.Linear qualified as Data

data ByteStream m r
    = Empty r
    | Chunk !ByteString (ByteStream m r)
    | Go (m (ByteStream m r))

instance Data.Functor m => Data.Functor (ByteStream m) where
    fmap ::
        forall a b.
        (a %1 -> b) ->
        ByteStream m a %1 ->
        ByteStream m b
    fmap f (Empty r) = Empty (f r)
    fmap f (Chunk bs rest) = Chunk bs (Data.fmap f rest)
    fmap f (Go m) = Go (Data.fmap (Data.fmap f) m)
    {-# INLINEABLE fmap #-}

instance Control.Functor m => Control.Functor (ByteStream m) where
    fmap ::
        forall a b.
        (a %1 -> b) %1 ->
        ByteStream m a %1 ->
        ByteStream m b
    fmap f (Empty r) = Empty (f r)
    fmap f (Chunk bs rest) = Chunk bs (Control.fmap f rest)
    fmap f (Go m) = Go (Control.fmap (Control.fmap f) m)
    {-# INLINEABLE fmap #-}

instance Control.Functor m => Data.Applicative (ByteStream m) where
    pure :: a -> ByteStream m a
    pure a = Empty a
    {-# INLINE pure #-}

    (<*>) ::
        forall a b.
        ByteStream m (a %1 -> b) %1 ->
        ByteStream m a %1 ->
        ByteStream m b
    (Empty f) <*> stream = Control.fmap f stream
    (Chunk bs rest) <*> stream = Chunk bs (rest Data.<*> stream)
    (Go m) <*> stream = Go (Control.fmap (Data.<*> stream) m)
    {-# INLINEABLE (<*>) #-}

instance Control.Functor m => Control.Applicative (ByteStream m) where
    pure :: a %1 -> ByteStream m a
    pure a = Empty a
    {-# INLINE pure #-}

    (<*>) ::
        forall a b.
        ByteStream m (a %1 -> b) %1 ->
        ByteStream m a %1 ->
        ByteStream m b
    (Empty f) <*> stream = Control.fmap f stream
    (Chunk bs rest) <*> stream = Chunk bs (rest Control.<*> stream)
    (Go m) <*> stream = Go (Control.fmap (Control.<*> stream) m)
    {-# INLINEABLE (<*>) #-}

instance Control.Functor m => Control.Monad (ByteStream m) where
    (>>=) ::
        forall a b.
        ByteStream m a %1 ->
        (a %1 -> ByteStream m b) %1 ->
        ByteStream m b
    (Empty a) >>= f = f a
    (Chunk bs rest) >>= f = Chunk bs (rest Control.>>= f)
    (Go m) >>= f = Go (Control.fmap (Control.>>= f) m)
    {-# INLINEABLE (>>=) #-}

instance Control.MonadTrans ByteStream where
    lift ::
        forall m a.
        Control.Functor m =>
        m a %1 ->
        ByteStream m a
    lift m = Go (Control.fmap Empty m)
    {-# INLINEABLE lift #-}