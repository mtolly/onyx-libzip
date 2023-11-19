module Main (main) where

import           Data.Functor                       (void)
import           Data.Maybe                         (fromJust)
import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PreProcess
import           Distribution.Simple.Setup          (ConfigFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo,
                                                     emptyHookedBuildInfo)
import           Distribution.Types.Library         (Library (..))
import           System.Directory                   (createDirectoryIfMissing,
                                                     getCurrentDirectory)
import           System.Process                     (CreateProcess (..),
                                                     callProcess, proc,
                                                     readCreateProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = myPreConf, confHook = myConfHook }

libzipCMakeFlags :: [String]
libzipCMakeFlags =
  [ "-DLIBZIP_DO_INSTALL=OFF"
  , "-DBUILD_SHARED_LIBS=OFF"
  -- we only need DEFLATE, I think; if these are enabled they need extra libs
  , "-DENABLE_BZIP2=OFF"
  , "-DENABLE_LZMA=OFF"
  , "-DENABLE_ZSTD=OFF"
  ]

-- Run cmake and make in the library folder to produce the static library
myPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
myPreConf _ _ = do
  let buildDir = "cbits/libzip-1.10.1/build"
  createDirectoryIfMissing False buildDir
  void $ readCreateProcess (proc "cmake" (".." : libzipCMakeFlags)) { cwd = Just buildDir } ""
  void $ readCreateProcess (proc "make" []) { cwd = Just buildDir } ""
  -- Return empty HookedBuildInfo as we are not modifying build info here.
  return emptyHookedBuildInfo

-- Add the directories containing the header file and the static library to the package.
-- Need to jump through hoops because a relative folder is not permitted in extra-lib-dirs
myConfHook
  :: (PD.GenericPackageDescription, PD.HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  return localBuildInfo
    { localPkgDescr = packageDescription
      { PD.library = Just library
        { PD.libBuildInfo = libraryBuildInfo
          { PD.includeDirs
            = (dir ++ "/cbits/libzip-1.10.1/lib") -- zip.h
            : (dir ++ "/cbits/libzip-1.10.1/build") -- zipconf.h
            : PD.includeDirs libraryBuildInfo
          , PD.extraLibDirs = (dir ++ "/cbits/libzip-1.10.1/build/lib") : PD.extraLibDirs libraryBuildInfo
          }
        }
      }
    }
