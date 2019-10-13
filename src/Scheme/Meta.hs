{-# LANGUAGE QuasiQuotes #-}

module Scheme.Meta (
  littleSchemePath
  , defaultLibraryPath
  , stdlibContent
) where
import Scheme.Literal (litFile)

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

defaultLibraryPath :: IO FilePath
defaultLibraryPath = do
  path <- littleSchemePath
  return $ path </> "lib"

littleSchemePath :: IO FilePath
littleSchemePath = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".little-scheme"

stdlibContent :: String
stdlibContent = [litFile|lib/stdlib.scm|]
