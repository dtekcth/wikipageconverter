{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module           : Data.WikiPageConverter.Revision
   Copyright        : Copyright (C) 2016 Jacob Jonsson
   License          : BSD 3

   Maintainer       : Jacob Jonsson <jassob@dtek.se>
   Stability        : alpha

   Type for representing a revision of a wiki page,
   primarily for exporting PmWiki pages.
   Contains functions for parsing JSON files to revisions. -}

module Data.WikiPageConverter.Revision ( Revision(..), toRevision ) where

import Prelude hiding (lookup, readFile, map)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe ( fromJust )
import Data.HashMap.Strict ( lookup )
import Data.Text ( Text, unpack )
import GHC.Generics

-- | Revision type. Represents a Page revision in a wiki
data Revision = Revision
  { diff    :: Int
  , author  :: Text
  , comment :: Text
  , content :: Text
  } deriving (Generic, Show)

-- | Tries to parse a JSON object to Revision.
--   Only accepts an object that contains diff,
--   author, comment and content fields.
toRevision :: Value -> Maybe Revision
toRevision (Object o) = Just Revision
  { diff = (read . unpack . f . fjL) "diff"
  , author = (f . fjL) "author"
  , comment = (f . fjL) "comment"
  , content = (f . fjL) "content"
  }
  where fjL = fromJust . flip lookup o
        f (String s) = s
        f _ = error "toRevision: can not extract string from object"

toRevision _ = error "toRevision: invalid json object"

instance ToJSON Revision where
  -- | Encodes a Revision value to a JSON encoded string
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Revision where
  -- | Parse a JSON object to a Revision
  parseJSON (Object v) = Revision <$>
    v .: "diff" <*>
    v .: "author" <*>
    v .: "comment" <*>
    v .: "content"
  parseJSON t = typeMismatch "Not a revision object" t
