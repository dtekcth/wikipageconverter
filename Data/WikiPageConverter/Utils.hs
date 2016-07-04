{- |
   Module           : Data.PmWikiExporter.Utils
   Copyright        : Copyright (C) 2016 Jacob Jonsson
   License          : BSD 3

   Maintainer       : Jacob Jonsson <jassob@dtek.se>
   Stability        : alpha

   Type for representing a revision of a wiki page,
   primarily for exporting PmWiki pages.
   Contains functions for parsing JSON files to revisions. -}

module Data.WikiPageConverter.Utils where

import Control.Monad ( liftM )

import Prelude hiding ( map, lookup, readFile )
import Data.Aeson ( decode )
import Data.ByteString.Lazy ( readFile )
import Data.HashMap.Strict ( HashMap, lookup, empty )
import Data.Text ( pack, unpack, Text )
import Text.Pandoc ( readHtml, def, writeMarkdown )

import Data.WikiPageConverter.Revision

-- | Reads an exported pmwiki page that is encoded in JSON and parses it
--   a list of Revisions, ordered chronologically.
findRevisions :: String -> IO [Revision]
findRevisions f = let
  toList :: Maybe (HashMap String Revision) -> [Revision]
  toList = revsToList . justOrEmpty

  justOrEmpty :: Maybe (HashMap k v) -> HashMap k v
  justOrEmpty Nothing  = empty
  justOrEmpty (Just m) = m

  in liftM toList (readExportJSONFile f)

-- | Translates the content of a Revision from HTML to Markdown
translateRevision :: Revision -> Revision
translateRevision r = r { content = newContent }
  where newContent = translateHtmlToMarkdown (content r)

-- | Reads HTML in a content and returns the same content coded in Markdown
translateHtmlToMarkdown :: Text -> Text
translateHtmlToMarkdown s = case readHtml def s' of
  Right pandoc     -> pack (writeMarkdown def pandoc)
  Left  _          -> error "translateHtmlToMarkdown: Parsing error"
  where s' = unpack s

-- | Translates the map to a list in chronological order
revsToList :: HashMap String a -> [a]
revsToList = reverse . flip revsToList' 0

-- | Starts from an index and appends each value to a list.
--   When there are no more values it simply returns the list.
--   Expects a hash map with numerical keys, represented as strings.
revsToList' :: (Show a, Num a) => HashMap String value -> a -> [value]
revsToList' revMap i =
  case lookup (show i) revMap of
    (Just rev) -> rev : revsToList' revMap (i + 1)
    Nothing    -> []

-- | Reads a file consisting of JSON encoded Revisions and returns it as
--   a HashMap of Revision types. The keys are string representations of
--   numbers starting at 0.
readExportJSONFile :: FilePath -> IO (Maybe (HashMap String Revision))
readExportJSONFile f = liftM decode (readFile f)
