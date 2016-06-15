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

findRevisions :: String -> IO (HM.HashMap String Revision)
findRevisions = readPmExportFile

printHelp :: IO ()
printHelp = undefined

commitRevs :: HM.HashMap String Revision -> String -> IO ()
commitRevs map dest = undefined

commitRev :: Revision -> String -> String -> IO ()
commitRev rev author msg = undefined
