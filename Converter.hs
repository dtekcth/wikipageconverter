{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- |

module Converter where

import System.Environment
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.Maybe
import Data.String.Conversions
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as I
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

toRevision :: Value -> Revision
toRevision (Object o) = Revision
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

parseAndWrite :: FilePath -> FilePath -> T.Text -> IO ()
parseAndWrite i o tit= do
  hm <- readPmExportFile i
  (I.writeFile o . makeMediaWikiXML . revisionsToXML tit) hm

translateHtmlToMediawiki :: T.Text -> T.Text
translateHtmlToMediawiki s = case (readHtml def s') of
  Left pandocError -> error "FEEEEL i parsningen"
  Right pandoc     -> T.pack $ writeMediaWiki def pandoc
  where s' = T.unpack s

translateRevision :: Revision -> Revision
translateRevision r = r { content = newContent }
  where newContent = translateHtmlToMediawiki (content r)

makeMediaWikiXML :: T.Text -> T.Text
makeMediaWikiXML t = "<mediawiki xmlns=\"http://www.mediawiki.org/xml/export-0.10/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.mediawiki.org/xml/export-0.10/ http://www.mediawiki.org/xml/export-0.10.xsd\" version=\"0.10\" xml:lang=\"sv\">\n" .+
  t .+
  "</mediawiki>"
  where (.+) = T.append

revisionsToXML :: T.Text -> HM.HashMap String Revision -> T.Text
revisionsToXML tit map =  "\t<page>\n" .+
  "\t\t<title>" .+ tit .+ "</title>\n" .+
  revisions .+
  "\t</page>"
  where revisions = HM.foldl'(flip ((.+) .  revisionToXML . translateRevision)) "" map
        (.+) = T.append

dropSubstring :: T.Text -> T.Text -> T.Text
dropSubstring s0 s1 = T.append (decode h) (decode $ C.drop (T.length s0) t)
  where (h, t) = C.breakSubstring s0' s1'
        s0' = E.encodeUtf8 s0
        s1' = E.encodeUtf8 s1
        decode = E.decodeUtf8

dropSubstringB :: B.ByteString -> B.ByteString -> B.ByteString
dropSubstringB s0 s1 = B.append h (C.drop (B.length s0) s1)
  where (h, t) = B.breakSubstring s0 s1

revisionToXML :: Revision -> T.Text
revisionToXML r = "\t<revision>\n" .+
  "\t\t<timestamp>" .+ dif .+ "</timestamp>\n" .+
  "\t\t<contributor><username>" .+ auth .+ "</username></contributor>\n" .+
  "\t\t<comment>" .+ com .+ "</comment>\n" .+
  "\t\t<text xml:space=\"preserve\" bytes=\"" .+ size .+ "\" >\n" .+
  con
  .+ "\n\t\t</text>\n" .+
  "\t</revision>\n"
  where dif = (p . show . diff) r
        auth = author r
        com = comment r
        con = content r
        size = (p . show . T.length) con
        p = T.pack
        (.+) = T.append

readPmExportFile :: FilePath -> IO (HM.HashMap String Revision)
readPmExportFile f = liftM readPmExportFile' (L.readFile f)

readPmExportFile' :: L.ByteString -> HM.HashMap String Revision
readPmExportFile' = HM.map toRevision . decode'
   where decode' = fromJust . decode :: L.ByteString -> HM.HashMap String Value


-- TEST DATA
s = Object (HM.fromList [("diff",String "1411474613"),("content",String "<h2>Jassob</h2>\n\n<div><img width=\"600px\" src=\"http://dtek.se/wiki/uploads/jassob.jpg\" alt=\"\" title=\"\"></div>\n\n<div class=\"vspace\"></div>\n<h2>Om Jassob:</h2>\n\n<ul>\n<li>Inskriven 2013\n</li>\n<li>Ordf\246rande, DNollK 2014\n</li>\n<li>Sitter enligt linjeledningen som ordf\246rande i f\246reningen \"Jacob och g\228nget\".\n</li>\n<li>F\229r CSN\n</li>\n</ul>\n<div class=\"vspace\"></div>\n<h2>Citat</h2>\n\n<ul><li>\"Man kan aldrig ha f\246r f\229 v\228nner!\"\n</li></ul>\n\n\n"),("author",String "alpingm"),("comment",String "")])
