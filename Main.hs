{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- | Main module.
-- In this module we use the readPmExportFile and lets it

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
bootstrap args
  | length args < 2 = printHelp
  | otherwise = do
      revs <- findRevisions (head args)
      commitRevs revs (second args)
        where second = head . tail

-- | Reads an exported pmwiki page that is encoded in JSON and parses it
-- a list of Revisions, ordered chronologically.
findRevisions :: String -> IO [Revision]
findRevisions f = readPmExportFile f >>= (return . revsToList)

-- | Prints the help text
printHelp :: IO ()
printHelp = undefined

-- | Recursive function to save and commit each revision of a page.
commitRevs :: [Revision] -> FilePath -> IO ()
commitRevs [] _ = putStrLn "Done!"
commitRevs (r:revs) dest = do
  commitRev r dest
  commitRevs revs dest
  where r' = translateRevision r

-- | The actual workhorse.
-- This function writes the binary stream to a file (encoded in utf8, of course)
-- and then calls Git commands via callCommand.
commitRev :: Revision -> FilePath -> IO ()
commitRev rev path = let encodeContent = E.encodeUtf8 . content
  in do
  -- Write the content of rev to path
  C.writeFile path (encodeContent rev)

  -- Adds the file to git index
  callCommand ("git add " ++ path)

  -- Commits the index
  callCommand $ T.unpack ("git commit -m \"" <+> msg <+> "\""
                          <+> " --author=\"" <+> commitAuthor <+> "\""
                          <+> " --date=\"" <+> commitDate <+> " +0200\"")

    -- If there are no comment for this commit, supply a dummy "Updated $page"
    -- TODO: remove directory parts and only print out file name.
    where msg = if T.length (comment rev) == 0
            then T.append "Updated " (T.pack path)
            else comment rev

          -- Creates a git author of the format: "name
          -- <name@student.chalmers.se>" since we know that the users
          -- on the old wiki logged in with their cids.
          commitAuthor = author rev <+> " <" <+> author rev <+> "@student.chalmers.se>"
          -- Creates a commit date entry that should be the time when
          -- the revision was created (retrieved from the diff field
          -- in the revision.
          commitDate = (T.pack . show . diff) rev

          -- Convenience rename of function to save characters.
          (<+>) = T.append

-- | Translates the map to a list in chronological order
revsToList :: HM.HashMap String a -> [a]
revsToList = reverse . (flip revsToList' 0)

-- | Starts from an index and appends each value to a list.
-- When there are no more values it simply returns the list.
-- Expects a hash map with numerical keys, represented as strings.
revsToList' :: (Show a, Num a) => HM.HashMap String value -> a -> [value]
revsToList' map i = case HM.lookup (show i) map of
                  (Just rev) -> rev : revsToList' map (i + 1)
                  Nothing  -> []
