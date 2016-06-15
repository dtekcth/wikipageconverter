{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- |

module Main where

import System.FilePath
import System.Environment
import System.Process
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Converter

main = do
  args <- getArgs
  bootstrap args

bootstrap :: [String] -> IO ()
bootstrap args | length args < 2 = printHelp
               | otherwise = do
                   revs <- findRevisions (head args)
                   commitRevs revs (second args)
                     where second = head . tail

findRevisions :: String -> IO [Revision]
findRevisions f = readPmExportFile f >>= (return . revsToList)

printHelp :: IO ()
printHelp = undefined

commitRevs :: [Revision] -> FilePath -> IO ()
commitRevs (r:revs) dest = do
  commitRev r dest
  commitRevs revs dest

commitRev :: Revision -> FilePath -> IO ()
commitRev rev path = let encodeContent = E.encodeUtf8 . content
                         (<+>) = T.append
  in do
  C.writeFile path (encodeContent rev)
  callCommand ("git add " ++ path)
  callCommand $ T.unpack ("git commit -m " <+> msg
                        <+> " --author=" <+> commitAuthor
                        <+> " --date=" <+> commitDate <+> " +0200")
    where msg = comment rev
          commitAuthor = author rev
          commitDate = (T.pack . show . diff) rev

-- | Translates the map to a list
revsToList :: HM.HashMap String Revision -> [Revision]
revsToList = reverse . (flip revsToList' 0)

revsToList' :: (Show a, Num a) => HM.HashMap String Revision -> a -> [Revision]
revsToList' map i = case HM.lookup (show i) map of
                  (Just rev) -> rev : revsToList' map (i + 1)
                  Nothing  -> []
