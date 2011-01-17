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
    
postCopyChi :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyChi args cflags pd lbi =
  copyFiles deafening (fromFlag (copyDistPref cflags)) [(buildDir lbi, "Text/Antlrc/Lexer.chi")]  

postInstChi :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postInstChi args iflags pd lbi = do
  let InstallDirs { libdir = libPref } = absoluteInstallDirs pd lbi NoCopyDest
  installOrdinaryFiles deafening libPref [(buildDir lbi, "Text/Antlrc/Lexer.chi")]
