-- Workaround around http://hackage.haskell.org/trac/hackage/ticket/48
-- 2. The .chi files need to be installed

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.FilePath

main = defaultMainWithHooks $ simpleUserHooks
    { postCopy = postCopyChi,
      postInst = postInstChi
    }

getLibDir :: PackageDescription -> LocalBuildInfo -> String
getLibDir pd lbi = libdir (absoluteInstallDirs pd lbi NoCopyDest)

copyChiFiles :: String -> LocalBuildInfo -> IO ()
copyChiFiles destLibDir lbi =
  installOrdinaryFiles deafening destLibDir [(buildDir lbi, "Text/Antlrc/Lexer.chi")]
    
postCopyChi :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyChi args cflags pd lbi =
  copyChiFiles ((\(CopyTo copyDest) -> copyDest) (fromFlag (copyDest cflags)) ++ tail (getLibDir pd lbi)) lbi

postInstChi :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postInstChi _ iflags pd lbi = copyChiFiles (getLibDir pd lbi) lbi
