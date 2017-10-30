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

import Prelude hiding ( readFile )
import Data.Aeson ( decode )
import Data.ByteString.Lazy ( readFile )
import Data.Text ( pack, unpack, Text )
import Text.Pandoc ( WriterOptions, Pandoc, readHtml, def )

import Data.WikiPageConverter.Revision

type Writer = WriterOptions -> Pandoc -> String

-- | Reads an exported pmwiki page that is encoded in JSON and parses it
--   a list of Revisions, ordered chronologically.
findRevisionsUnsafe :: FilePath -> IO [Revision]
findRevisionsUnsafe f = fromJust <$> findRevisions f
  where fromJust :: Maybe a -> a
        fromJust (Just x) = x
        fromJust Nothing  = error "fromJust: Not defined for Nothing"

-- | Reads a file consisting of JSON encoded Revisions and returns it as
--   a list of Revision types.
findRevisions :: FilePath -> IO (Maybe [Revision])
findRevisions f = liftM decode (readFile f)

-- | Translates the content of a Revision from HTML to Markdown
translateRevision :: Revision -> Writer -> WriterOptions -> Revision
translateRevision r w opts = r { content = newContent }
  where newContent = translateHtmlToFormat (content r) w opts

-- | Reads HTML in a content and returns the same content coded in Markdown
translateHtmlToFormat :: Text -> Writer -> WriterOptions -> Text
translateHtmlToFormat s w opts = case readHtml def s' of
  Right pandoc     -> pack (w opts pandoc)
  Left  _          -> error "translateHtmlToMarkdown: Parsing error"
  where s' = unpack s
