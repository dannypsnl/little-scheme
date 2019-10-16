{-# LANGUAGE TemplateHaskell #-}

module Scheme.Meta (
  littleSchemePath
  , defaultLibraryPath
  , stdlibContent
) where
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
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

stdlibContent :: ByteString
stdlibContent = $(makeRelativeToProject "lib/stdlib.scm" >>= embedFile)
