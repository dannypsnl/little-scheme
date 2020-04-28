import Control.Monad
import Data.Maybe
import Distribution.Simple
import System.Directory
import System.Environment
import System.FilePath

main = do
  defaultMain
  inCI <- lookupEnv "GITHUB_WORKSPACE"
  when (isNothing inCI) (do
    putStrLn "Not CI environment, install scheme library"
    initLittleScheme
    putStrLn "scheme library installed"
    )

initLittleScheme :: IO ()
initLittleScheme = do
  target <- defaultLibraryPath
  createDirectoryIfMissing True target
  copyFile "lib/stdlib.scm" (target </> "stdlib.scm")

defaultLibraryPath :: IO FilePath
defaultLibraryPath = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".little-scheme" </> "lib"
