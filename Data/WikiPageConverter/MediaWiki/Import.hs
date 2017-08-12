
module Data.WikiPageConverter.MediaWiki.Import where

import           Control.Exception (SomeException, handle)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as HM
import           System.Exit (ExitCode(..), exitWith)

import           Data.WikiPageConverter.MediaWiki.Types

-- | In-memory database of MediaWiki namespaces.
-- To be used as reference when creating import files.
type NamespaceDB = Map Int Namespace

-- | In-memory database of MediaWiki users.
-- To be used as reference when creating import files.
type UserDB = Map Int Author

-- | Config to use when creating the import XML's.
-- This import needs to keep track of everything
-- that needs to be known during the import.
-- The plan for this data type is to be used as a
-- State during the creation of the import files.
data ImportConfig = IConfig { lastPageId :: Int
                            , lastRevId :: Int
                            , namespaces :: NamespaceDB -- Immutable
                            , users :: UserDB -- Immutable
                            }

-- | Read a stored UserDB from file f
readUserDB :: FilePath -> IO UserDB
readUserDB f = readFileWithError f "user"

-- | Read a stored NamespaceDB from file f
readNamespaceDB :: FilePath -> IO NamespaceDB
readNamespaceDB f = readFileWithError f "namespace"

readFileWithError :: Read a => FilePath -> String -> IO a
readFileWithError f filetype = handle printError $ read <$> readFile f
  where printError :: SomeException -> IO a
        printError _ = do
          putStrLn $ "Could not read the " ++ filetype ++ " DB file, "
            ++ "ensure that it exists and it contains only readable haskell data."
          exitWith $ ExitFailure 1

-- | Stores data that can be both read and shown in a file.
-- The Read a constraint is to ensure that we do not store
-- anything that can't be read back.
storeInFile :: (Show a, Read a) => a -> FilePath -> IO ()
storeInFile db f = writeFile f $ show db
