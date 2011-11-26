-- Copyright (c)2010-2011, Mark Wright.  All rights reserved.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.Antlrc.Lexer where

#include "antlr3lexer.h"

import C2HS
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe
import Foreign.C
import Control.Monad
import Control.Applicative ((<$>))

{#context lib="antlr3c"#}

-- | Lexer token struct.
{#pointer *ANTLR3_COMMON_TOKEN as CommonToken newtype#}

-- | Lexer input stream struct.
{#pointer *ANTLR3_INPUT_STREAM as InputStream newtype#}

-- | Lexer struct.
{#pointer *ANTLR3_LEXER as Lexer newtype#}

-- | Cast from a pointer to an input stream to an input stream.
toInputStream :: Ptr InputStream -> InputStream
toInputStream = InputStream . castPtr

-- | Cast from a pointer to a token to a token.
toCommonToken :: Ptr CommonToken -> CommonToken
toCommonToken = CommonToken . castPtr

-- | Cast from a token to a pointer to a token.
fromCommonToken :: CommonToken -> Ptr b
fromCommonToken (CommonToken x) = castPtr x

-- The C function definitions are in lexerc.c
#c
ANTLR3_COMMON_TOKEN *LT(ANTLR3_INPUT_STREAM *input, int lti);
#endc

-- | Lookahead in the input stream at the token at the specified
--   positive offset, where:
--
-- > LT input 1 
--
--   is the current token.  Or a negative offset may be specified, where: 
--
-- > LT input (-1) 
--
--   is the previous token.
--
-- > foreign export ccall isUnsignedInt :: Ptr InputStream -> IO Bool
-- > isUnsignedInt input =
-- >   do token1 <- lT input 1 >>= tokenGetType
-- >      if token1 /= UNSIGNED
-- >        then return False
-- >        else 
-- >        do 
-- >          token2 <- lT input 2 >>= tokenGetType
-- >          return ((token2 /= CHAR) && (token2 /= SHORT) && (token2 /= LONG))
--
{#fun LT as lT
 { toInputStream `Ptr (InputStream)',
   `Int' } -> `Ptr (CommonToken)' fromCommonToken#}

-- | Pointer to an ANTLR string.
{#pointer *ANTLR3_STRING as AntlrString newtype#}

-- | Cast from an ANTLR string to a pointer to an ANTLR string.
fromAntlrString :: AntlrString -> Ptr b
fromAntlrString (AntlrString x) = castPtr x

#c
ANTLR3_STRING *tokenGetAntlrString(ANTLR3_COMMON_TOKEN *token);
#endc

-- | Obtain the token name ANTLR string for the specified token.
--
-- > tokenGetAntlrString token
--
--   For identifier tokens, the token string is interesting.  For
--   other tokens such as operator tokens, the token string is
--   uninteresting, and may not be present, the token identifier enum 
--   should be used instead.
{#fun tokenGetAntlrString
 { toCommonToken `Ptr (CommonToken)' } -> `Ptr (AntlrString)' fromAntlrString#}

-- | Convert an ANTLR string to a Maybe String. 
fromAntlrStringToMaybeString :: AntlrString -> IO (Maybe String)
fromAntlrStringToMaybeString (AntlrString x) = 
  if x == nullPtr
  then return Nothing
  else 
    {#get ANTLR3_STRING->chars#} x >>= \c ->
    {#get ANTLR3_STRING->len#} x >>= \l ->
    peekCStringLen ((castPtr c), (fromIntegral l)) >>= \s ->
    return (Just s)

-- | Obtain the token Maybe String for the specified token.
--   For identifier tokens, the token string is interesting.  For
--   other tokens such as operator tokens, the token string is
--   uninteresting, and may not be present, the token identifier enum 
--   should be used instead.
tokenGetTextMaybe :: Ptr (CommonToken) -> IO (Maybe String)
tokenGetTextMaybe c =
  tokenGetAntlrString c >>= \s ->
  fromAntlrStringToMaybeString (AntlrString s)

-- | Convert from an ANTLR string to a String.
--   Note: the peekCStringLen function does not say what will happen if the
--   C pointer is 0.
fromAntlrStringToString :: AntlrString -> IO String
fromAntlrStringToString (AntlrString x) = 
  {#get ANTLR3_STRING->chars#} x >>= \c ->
  {#get ANTLR3_STRING->len#} x >>= \l ->
  peekCStringLen ((castPtr c), (fromIntegral l)) >>= \s ->
  return s

-- | Obtain the token String for the specified token.
--   Note: the peekCStringLen function does not say what will happen if the
--   C pointer is 0.
--
-- > foreign export ccall saIntV :: Ptr CommonToken -> IO (StablePtr TermInfo)
-- > saIntV token =
-- >   do
-- >     -- read the IntV integer value from the token text into n
-- >     t <- tokenGetText token
-- >     n <- readIO t
-- >     -- obtain the source code line and charPosition from the token
-- >     l <- tokenGetLine token
-- >     c <- tokenGetCharPositionInLine token
-- >     -- return the term, which is TmZero, or TmSucc TmZero, or TmSucc (TmSucc (...TmSucc TmZero))
-- >     newStablePtr (intV (Info l c) n)
--
tokenGetText :: Ptr (CommonToken) -> IO String
tokenGetText c =
  tokenGetAntlrString c >>= \s ->
  fromAntlrStringToString (AntlrString s)

-- | Obtain the token identifier for the specified token.
--
-- > foreign export ccall isInt :: Ptr InputStream -> IO Bool
-- > isInt input =
-- >   do 
-- >     token1 <- lT input 1 >>= tokenGetType
-- >     return (token1 == INT)
--
tokenGetType :: (Enum e) => Ptr (CommonToken) -> IO e
tokenGetType token = {#get ANTLR3_COMMON_TOKEN->type#} token >>= return . cToEnum

-- | Obtain the character position in the source code line of where the token
--   was read, for non-imaginary tokens.
--
-- > foreign export ccall saTrue :: Ptr CommonToken -> IO (StablePtr TermInfo)
-- > saTrue token =
-- >   do
-- >     -- obtain the source code line and charPosition from the token
-- >     l <- tokenGetLine token
-- >     c <- tokenGetCharPositionInLine token
-- >     -- return the TmTrue term
-- >     newStablePtr (TmTrue (Info l c))
--
tokenGetCharPositionInLine :: Ptr (CommonToken) -> IO Int
tokenGetCharPositionInLine token = {#get ANTLR3_COMMON_TOKEN->charPosition#} token >>= return . cIntConv

-- | Obtain the the source code line of where the token was read, for non-imaginary tokens.
--
-- > foreign export ccall saFalse :: Ptr CommonToken -> IO (StablePtr TermInfo)
-- > saFalse token =
-- >   do
-- >     -- obtain the source code line and charPosition from the token
-- >     l <- tokenGetLine token
-- >     c <- tokenGetCharPositionInLine token
-- >     -- return the TmFalse term
-- >     newStablePtr (TmFalse (Info l c))
--
tokenGetLine :: Ptr (CommonToken) -> IO Int
tokenGetLine token = {#get ANTLR3_COMMON_TOKEN->line#} token >>= return . cIntConv
