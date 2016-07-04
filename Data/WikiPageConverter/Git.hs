{-# LANGUAGE OverloadedStrings #-}

{- |
   Module           : Data.WikiPageConverter.Git
   Copyright        : Copyright (C) 2016 Jacob Jonsson
   License          : BSD 3

   Maintainer       : Jacob Jonsson <jassob@dtek.se>
   Stability        : alpha

   A module for working with git storage backends, such as Gitit.
   -}

module Data.WikiPageConverter.Git ( commitRevs, commitRev ) where

import Prelude hiding ( length )
import System.FilePath ( takeDirectory, takeFileName )
import System.IO.Error ( catchIOError, ioError )
import Control.Exception ( throwIO, try )
import Data.FileStore
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.ByteString.Lazy ( toStrict, fromStrict )
import Data.Text ( append, pack, unpack, length )
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

import qualified Data.WikiPageConverter.Revision as R

-- | Recursive function to save and commit each revision of a page.
commitRevs :: [R.Revision] -> FilePath -> IO ()
commitRevs [] f = putStrLn $ "Done saving " ++ f ++ "!"
commitRevs (r:revs) dest = let
  commit :: IO ()
  commit = commitRev r dest

  handler :: FileStoreError -> IO ()
  handler e | e == Unchanged = do
                putStrLn $ "Git reported " ++ dest ++ " unchanged, not committing."
                commitRevs revs dest
            | otherwise = throwIO e
  in do
  result <- try commit
  case result of
    (Left e)       -> handler e
    (Right ())     -> commitRevs revs dest


commitRev :: R.Revision -> FilePath -> IO ()
commitRev rev path =
  let encodeContent = E.encodeUtf8 . R.content

      repo = gitFileStore (takeDirectory path)

      date' = (posixSecondsToUTCTime . toEnum . R.diff) rev

  in saveWithDate repo (takeFileName path) author date' (unpack desc) (encodeContent rev)

  where author = mkChalmersStudent ((unpack . R.author) rev)
        mkChalmersStudent "" = mkChalmersStudent "d2016Unknown"
        mkChalmersStudent cid = Author cid
          ("<" ++ cid ++ "@student.chalmers.se>")


        desc = if length (R.comment rev) == 0
               then append "Updated " (pack path)
               else R.comment rev

instance Contents B.ByteString where
  toByteString = fromStrict
  fromByteString = toStrict
