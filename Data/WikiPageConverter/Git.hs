{-# LANGUAGE OverloadedStrings #-}

{- |
   Module           : Data.WikiPageConverter.Git
   Copyright        : Copyright (C) 2016 Jacob Jonsson
   License          : BSD 3

   Maintainer       : Jacob Jonsson <jassob@dtek.se>
   Stability        : alpha

   A module for working with git storage backends, such as Gitit.
   -}

module Data.WikiPageConverter.Git ( commitRevs ) where

import Prelude hiding ( length )
import System.FilePath ( takeDirectory )
import Data.FileStore
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.ByteString.Lazy ( toStrict, fromStrict )
import Data.Text ( append, pack, unpack, length )
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

import qualified Data.WikiPageConverter.Revision as R

-- | Recursive function to save and commit each revision of a page.
commitRevs :: [R.Revision] -> FilePath -> IO ()
commitRevs [] _ = putStrLn "Done!"
commitRevs (r:revs) dest = do
  commitRev r dest
  commitRevs revs dest

-- | The actual workhorse.
-- This function writes the binary stream to a file (encoded in utf8, of course)
-- and then calls Git commands via callCommand.
commitRev :: R.Revision -> FilePath -> IO ()
commitRev rev path =
  let encodeContent = E.encodeUtf8 . R.content
      repo = gitFileStore (takeDirectory path)
      date' = (posixSecondsToUTCTime . toEnum . R.diff) rev
  in do
    saveWithDate repo path author date' (unpack desc) (encodeContent rev)

  where author = (mkChalmersStudent ((unpack . R.author) rev))
        mkChalmersStudent cid = Author cid
          ("<" ++ cid ++ "@student.chalmers.se>")
        desc = if length (R.comment rev) == 0
               then append "Updated " (pack path)
               else R.comment rev

instance Contents B.ByteString where
  toByteString = fromStrict
  fromByteString = toStrict
