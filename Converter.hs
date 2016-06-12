{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- |

module Converter where

import System.Environment
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Control.Monad
import GHC.Generics
import Text.Pandoc
import Text.Pandoc.Options

data Revision = Revision
  { diff :: Int
  , author :: T.Text
  , comment :: T.Text
  , content :: T.Text
  } deriving (Generic, Show)

toRevision :: Value -> Maybe Revision
toRevision (Object o) = Just Revision
  { diff = (read . T.unpack . f . fjL) "diff"
  , author = (f . fjL) "author"
  , comment = (f . fjL) "comment"
  , content = (f . fjL) "content"
  }
  where fjL = fromJust . flip HM.lookup o
        f (String s) = s

instance ToJSON Revision where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Revision where
  parseJSON (Object v) = Revision <$>
    v .: "diff" <*>
    v .: "author" <*>
    v .: "comment" <*>
    v .: "content"
  parseJSON t = typeMismatch "Not a revision object" t

translateHtmlToMarkdown :: T.Text -> T.Text
translateHtmlToMarkdown s = case readHtml def s' of
  Left pandocError -> error "FEEEEL i parsningen"
  Right pandoc     -> T.pack (writeMarkdown def pandoc)
  where s' = T.unpack s

translateRevision :: Revision -> Revision
translateRevision r = r { content = newContent }
  where newContent = translateHtmlToMarkdown (content r)

readPmExportFile :: FilePath -> IO (HM.HashMap String Revision)
readPmExportFile f = liftM readPmExportFile' (L.readFile f)

readPmExportFile' :: L.ByteString -> HM.HashMap String Revision
readPmExportFile' = HM.map fromRevision . decode'
   where decode' = fromJust . decode :: L.ByteString -> HM.HashMap String Value
         fromRevision = fromJust . toRevision


-- TEST DATA
s = Object (HM.fromList [("diff",String "1411474613"),("content",String "<h2>Jassob</h2>\n\n<div><img width=\"600px\" src=\"http://dtek.se/wiki/uploads/jassob.jpg\" alt=\"\" title=\"\"></div>\n\n<div class=\"vspace\"></div>\n<h2>Om Jassob:</h2>\n\n<ul>\n<li>Inskriven 2013\n</li>\n<li>Ordf\246rande, DNollK 2014\n</li>\n<li>Sitter enligt linjeledningen som ordf\246rande i f\246reningen \"Jacob och g\228nget\".\n</li>\n<li>F\229r CSN\n</li>\n</ul>\n<div class=\"vspace\"></div>\n<h2>Citat</h2>\n\n<ul><li>\"Man kan aldrig ha f\246r f\229 v\228nner!\"\n</li></ul>\n\n\n"),("author",String "alpingm"),("comment",String "")])
