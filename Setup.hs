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
postCopyChi _ _ = copyChiFiles

postInstChi :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postInstChi _ _ = copyChiFiles

copyChiFiles :: PackageDescription -> LocalBuildInfo -> IO ()
copyChiFiles pd lbi =
  installOrdinaryFiles deafening (libdir (absoluteInstallDirs pd lbi NoCopyDest)) [(buildDir lbi, "Text/Antlrc/Lexer.chi")]
