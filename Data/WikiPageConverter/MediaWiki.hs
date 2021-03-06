{-# LANGUAGE OverloadedStrings #-}
{- |
   Module           : Data.WikiPageConverter.MediaWiki
   Copyright        : Copyright (C) 2017 Jacob Jonsson
   License          : BSD 3

   Maintainer       : Jacob Jonsson <jassob@dtek.se>
   Stability        : alpha

   A module for exporting wiki pages to MediaWiki xml format.
-}

module Data.WikiPageConverter.MediaWiki where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.DateTime (fromSeconds)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import Data.List (intersperse)

import Data.WikiPageConverter.Utils
import Data.WikiPageConverter.Revision (Revision(..))
import qualified Data.WikiPageConverter.Revision as R

withXMLBase :: ([Revision] -> ByteString) -> String -> ([Revision] -> ByteString)
withXMLBase f title revs = mconcat $ intersperse ("\n")
  [ "<mediawiki xml:lang=\"sv\">"
  , "<page>"
  , "<title>" <> pack title <> "</title>"
  , f revs
  , "</page>"
  , "</mediawiki>"
  ]

revToXML :: Revision -> ByteString
revToXML (Revision time user com content) = mconcat $ intersperse ("\n")
  [ "<revision>"
  , "<timestamp>" <> (pack . show . fromSeconds) time <> "</timestamp>"
  , "<contributor><username>" <> encodeUtf8 user <> "</username></contributor>"
  , "<comment>" <> encodeUtf8 com <> "</comment>"
  , "<text>" <> encodeUtf8 content <> "</text>"
  , "</revision>"
  ]
