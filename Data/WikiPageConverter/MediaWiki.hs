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

import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Text.XML.Generator

import           Data.WikiPageConverter.Utils
import           Data.WikiPageConverter.Revision (Revision(..))
import qualified Data.WikiPageConverter.Revision as R
import qualified Data.WikiPageConverter.MediaWiki.Types as MWT

-- | Creates the <page> node and its data.
createPageXml :: ((Revision, Int) -> Xml Elem)
              -- ^ Revision to XML element function
              -> T.Text
              -- ^ Title
              -> MWT.Namespace
              -- ^ Namespace
              -> [Revision]
              -- ^ Revisions
              ->  Xml Elem
createPageXml f title ns revs =
  xelem "page" . xelems $
  [ xelem "title" title
  , xelem "ns" (pshow . MWT.key $ ns)
  ] <> zipWith (curry f) revs [1..]

mediaWikiDocInfo :: Xml Elem
mediaWikiDocInfo = xelemQ exportNs "mediawiki" $
  xattrs [ xattrQ xmlSchemaNs "schemaLocation" schemaLoc
         , xattr "version" "0.10" ]

  where exportNs    = namespace "" "http://www.mediawiki.org/xml/export-0.10"
        xmlSchemaNs = namespace "xsi" "http://www.w3.org/2001/XMLSchema-instance"
        schemaLoc   = "http://mediawiki.org/xml/export-0.10/ http://mediawiki.org/xml/export-0.10.xsd"


-- Text alias
pshow :: Show a => a -> T.Text
pshow = T.pack . show
