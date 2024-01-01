module Main (main) where

import           Data.Functor                       (void)
import           Data.Maybe                         (fromJust)
import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PreProcess
import           Distribution.Simple.Setup          (ConfigFlags, CopyFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo,
                                                     emptyHookedBuildInfo)
import           Distribution.Types.Library         (Library (..))
import           System.Directory                   (createDirectoryIfMissing,
                                                     getCurrentDirectory, copyFile)
import           System.Environment                 (setEnv)
import           System.Process                     (CreateProcess (..),
                                                     callProcess, proc,
                                                     readCreateProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = myPreConf, confHook = myConfHook, copyHook = myCopyHook }

libzipCMakeFlags :: [String]
libzipCMakeFlags =
  [ "-DLIBZIP_DO_INSTALL=OFF"
  , "-DBUILD_SHARED_LIBS=OFF"
  -- we only need DEFLATE, I think; if these are enabled they need extra libs
  , "-DENABLE_BZIP2=OFF"
  , "-DENABLE_LZMA=OFF"
  , "-DENABLE_ZSTD=OFF"
  ]

-- On my Stack system, for an external package such as a git reference, this gives me something like
-- $HOME/.stack/snapshots/x86_64-linux-tinfo6/fccfa9932e1adafc2949e626b62baeceff08655250fc8a4a10c9ee215c88d179/9.6.2/lib
staticLibOutputDir :: PackageDescription -> LocalBuildInfo -> FilePath
staticLibOutputDir packageDescription localBuildInfo
  = flibdir $ absoluteComponentInstallDirs
    packageDescription
    localBuildInfo
    (localUnitId localBuildInfo)
    NoCopyDest

-- Run cmake and make in the library folder to produce the static library
myPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
myPreConf _ _ = do
  let buildDir = "cbits/libzip-1.10.1/build"
  createDirectoryIfMissing False buildDir
  -- this is needed for some reason on older systems like Ubuntu 16.04. but newer ones maybe do it automatically.
  -- error you get otherwise is
  -- /usr/bin/ld.gold: error: /tmp/stack-eeeb9bc293444ad7/onyx-libzip-0.1.0.0/cbits/libzip-1.10.1/build/lib/libzip.a(zip_close.c.o): requires dynamic R_X86_64_PC32 reloc against 'malloc' which may overflow at runtime; recompile with -fPIC
  setEnv "CFLAGS" "-fPIC"
  -- need to specify make generator on mingw, otherwise default is ninja
  void $ readCreateProcess (proc "cmake" (".." : "-G" : "Unix Makefiles" : libzipCMakeFlags)) { cwd = Just buildDir } ""
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
          , PD.extraLibDirs
            = (dir ++ "/cbits/libzip-1.10.1/build/lib") -- this is needed while building this package
            : staticLibOutputDir packageDescription localBuildInfo -- this is the permanent place we copy to, needed while linking downstream
            : PD.extraLibDirs libraryBuildInfo
          }
        }
      }
    }

myCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
myCopyHook packageDescription localBuildInfo userHooks copyFlags = do
  copyHook simpleUserHooks packageDescription localBuildInfo userHooks copyFlags
  copyFile "cbits/libzip-1.10.1/build/lib/libzip.a"
    (staticLibOutputDir packageDescription localBuildInfo <> "/libzip.a")
