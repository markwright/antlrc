-- Copyright (c)2010, Mark Wright.  All rights reserved.

module Main (main) where
import Data.Enumerator
import qualified Data.ByteString as B8
import Control.Exception as E
import qualified Data.List as L
import Control.Monad (liftM, unless)
import System.IO
import System.IO.Error (isEOFError)
import System.Environment
import Text.Regex.Posix
import Data.Char

tokenLineConsComma :: (Int, B8.ByteString) -> (Int, B8.ByteString)
tokenLineConsComma (n, b) =
  (n + 1, b')
  where
    b' = if n == 0
           then indent `B8.append` b
           else c `B8.cons` (nl `B8.cons` (indent `B8.append` b))
    c = toEnum (fromEnum ',')
    nl = toEnum (fromEnum '\n')
    indent = B8.replicate 2 (toEnum (fromEnum ' '))

tokenLine :: (Int, B8.ByteString) -> (Int, B8.ByteString)
tokenLine (n, b) =
  let tl = b =~ "[^\']*" :: B8.ByteString
  in
    if B8.null tl
       then (n, B8.empty)
       else tokenLineConsComma (n, tl)

tokenLines :: Handle -> Int -> [B8.ByteString] -> IO Int
tokenLines h acc bytes =
  let (n, tl) = L.foldl' sumLines (acc, B8.empty) (zip [acc..] bytes)
      sumLines :: (Int, B8.ByteString) -> (Int, B8.ByteString) -> (Int, B8.ByteString)
      sumLines (m, s) (n, t) =
        (x, u)
        where
          (x, t') = tokenLine (n, t)
          u = s `B8.append` t'
  in do
     B8.hPutStr h tl
     return n

iterHandleLines :: Handle -> Iteratee B8.ByteString IO Int
iterHandleLines h = continue (step 0) where
  step acc EOF = yield acc EOF
  step acc (Chunks bytes) = Iteratee $ do
    eitherErr <- E.try $ tokenLines h acc bytes
    return $ case eitherErr of
      Left err -> Error err
      Right lines -> Continue (step lines)

enumHandleLines :: Handle -> Enumerator B8.ByteString IO a
enumHandleLines h = Iteratee . loop where
  loop (Continue k) = do
    eitherBytes <- E.try (B8.hGetLine h)
    case eitherBytes of
      Left err -> if isEOFError err
                     then return  (Continue k)
                     else return $ Error $ E.toException err
      Right bytes | B8.null bytes -> return (Continue k)
      Right bytes -> runIteratee (k (Chunks [bytes])) >>= loop
  loop step = return step

enumFileLines :: FilePath -> Enumerator B8.ByteString IO b
enumFileLines path s = Iteratee $ do
  eitherH <- E.try $ openFile path ReadMode
  case eitherH of
    Left err -> return $ Error err
    Right h -> finally
               (runIteratee (enumHandleLines h s))
               (hClose h)

filePaths :: [String] -> (FilePath, FilePath)
filePaths args
  | argc == 1 = (inputFilePath, outFile inputFilePath)
  | otherwise = (inputFilePath, args !! 1)
  where
    argc = L.length args
    inputFilePath = L.head args
    outFile :: String -> FilePath
    outFile s = (s =~ "[^\\.]*") ++ "Tokens.h"

header :: Handle -> FilePath -> IO ()
header h s =
  let grammarName = s =~ "[^\\.]*" :: String
      headerGuardName = toUpper (L.head grammarName) : tail grammarName ++ "Tokens_H"
      tokensEnumName = toUpper (L.head grammarName) : tail grammarName ++ "Tokens"
  in
    do
      hPutStrLn h $ "#ifndef " ++ headerGuardName
      hPutStrLn h $ "#define " ++ headerGuardName ++ "\n"
      hPutStrLn h "#include <antlr3commontoken.h>\n"
      hPutStrLn h "#undef EOF\n"
      hPutStrLn h $ "enum " ++ tokensEnumName ++ "\n{"

trailer :: Handle -> IO ()
trailer h = do
  hPutStrLn h "\n};\n"
  hPutStrLn h "#endif\n"

main :: IO Int
main = do
  args <- getArgs
  let argc = L.length args
  if (argc >= 1) && (argc <= 2)
    then do
      let (inputFilePath, outputFilePath) = filePaths args
          enum = enumFileLines inputFilePath
      eitherH <- E.try $ openFile outputFilePath WriteMode
      case eitherH of
        Left err ->
          putStrLn "Failed to open output file " >> ioError err >> return 1
        Right h ->
          finally
          (header h inputFilePath >> run_ (enum $$ iterHandleLines h) >> trailer h >> return 0)
          (hClose h)
    else do
      putStrLn "Usage: antlrcmkenums grammar.tokens [grammarTokens.h]"
      putStrLn "antlrcmkenums reads the grammar.tokens file that is generated from grammar.g by"
      putStrLn "ANTLR. antlrcmkenums outputs a C/C++ header file grammarTokens.h containing the"
      putStrLn "C/C++ definition of an enum grammarToeksn, with an enum element for each token"
      putStrLn "that has been assigned a name string."
      return 1

