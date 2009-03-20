module Paths_Bunch_Of_Words (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/di/.cabal/bin"
libdir     = "/home/di/.cabal/lib/Bunch-Of-Words-0.0.1/ghc-6.10.1"
datadir    = "/home/di/.cabal/share/Bunch-Of-Words-0.0.1"
libexecdir = "/home/di/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Bunch-Of-Words_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Bunch-Of-Words_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Bunch-Of-Words_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Bunch-Of-Words_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
