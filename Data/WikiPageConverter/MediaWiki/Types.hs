{-# LANGUAGE OverloadedStrings #-}

-- | Types for MediaWiki

module Data.WikiPageConverter.MediaWiki.Types where

import qualified Data.Text as T
import           Data.Thyme

-- | Data type for MediaWiki page namespaces
data Namespace =
  NS { key    :: Int
     , casing :: T.Text
     , name   :: T.Text
     } deriving (Read, Show)

-- | MediaWiki revision data type
data Revision =
  Rev { revId :: Int
      , timeStamp :: UTCTime
      , contributor :: Author
      , text :: T.Text
      , checksum :: T.Text
      }

-- | Author / contributor data type
data Author =
  Author { username :: String
         , authorId :: Int
         } deriving (Read, Show)
