{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeSynonymInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
-- Module      : Data.Convertible
-- Description : Classes for converting things.
--
module Convertible
  ( Convertible(convert)
  , Iso
  , over
  )
where

import           Prelude

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as Lazy
import qualified Data.ByteString.Lazy.Char8    as LazyChar8
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )


-- | Typeclass for things that can be converted unidirectionally.
--
-- This is formally known as "injectivity" (type a can be "injected" into b).
class Convertible a b where
    convert :: a -> b


-- | The class of isomorphic types.
class (Convertible a b, Convertible b a) => Iso a b


-- Some useful instances to get started:
instance Convertible ByteString      String          where convert = Text.unpack . decodeUtf8
instance Convertible ByteString      Lazy.ByteString where convert = Lazy.fromStrict
instance Convertible ByteString      Text            where convert = decodeUtf8
instance Convertible Lazy.ByteString ByteString      where convert = Lazy.toStrict
instance Convertible Lazy.ByteString String          where convert = Text.unpack . decodeUtf8 . Lazy.toStrict
instance Convertible Text            ByteString      where convert = encodeUtf8
instance Convertible Text            String          where convert = Text.unpack
instance Convertible String          Text            where convert = Text.pack
instance Convertible String          ByteString      where convert = encodeUtf8 . Text.pack
instance Convertible String          Lazy.ByteString where convert = LazyChar8.pack

instance Iso         ByteString      String
instance Iso         ByteString      Text
instance Iso         Lazy.ByteString ByteString
instance Iso         Text            String



-- | Apply a function via some other type.
--
-- >>> let dropHead str = drop 1 (str :: String)
-- >>> over dropHead ("abcd" :: Text)
-- "bcd"
over :: Iso a b => (b -> b) -> a -> a
over f = convert . f . convert
