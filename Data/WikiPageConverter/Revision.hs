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

module Data.WikiPageConverter.Revision ( Revision(..) ) where

import Prelude hiding (lookup, readFile, map)
import Data.Aeson
import Data.Aeson.Types
import Data.Text ( Text )
import GHC.Generics

-- | Revision type. Represents a Page revision in a wiki
data Revision = Revision
  { diff    :: Integer
  , author  :: Text
  , comment :: Text
  , content :: Text
  } deriving (Generic, Show)

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
