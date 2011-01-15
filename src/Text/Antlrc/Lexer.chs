-- Copyright (c)2010-2011, Mark Wright.  All rights reserved.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.Antlrc.Lexer where

#include "antlr3lexer.h"

import C2HS
import Foreign.C.Types
import Foreign.Storable
import Ptr
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
-- | positive offset, where:L 
-- | >>> LT inputStream 1 
-- | is the current token, or negative offset, where: 
-- | >>> LT inputstream -1 
-- | is the previous token.
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
-- | >>> tokenGetAntlrString token
-- | For identifier tokens, the token string is interesting.  For
-- | other tokens such as operator tokens, the token string is
-- | uninteresting, and may not be present, the token identifier enum 
-- | should be used instead.
{#fun tokenGetAntlrString
 { toCommonToken `Ptr (CommonToken)' } -> `Ptr (AntlrString)' fromAntlrString#}

-- | Convert an ANTLR string to a Maybe String. 
fromAntlrStringToMaybeString :: AntlrString -> IO (Maybe String)
fromAntlrStringToMaybeString (AntlrString x) = 
  if x == Ptr.nullPtr
  then return Nothing
  else 
    {#get ANTLR3_STRING->chars#} x >>= \c ->
    {#get ANTLR3_STRING->len#} x >>= \l ->
    peekCStringLen ((castPtr c), (fromIntegral l)) >>= \s ->
    return (Just s)

-- | Obtain the token Maybe String for the specified token.
-- | >>> tokenGetTextMaybe token
-- | For identifier tokens, the token string is interesting.  For
-- | other tokens such as operator tokens, the token string is
-- | uninteresting, and may not be present, the token identifier enum 
-- | should be used instead.
tokenGetTextMaybe :: Ptr (CommonToken) -> IO (Maybe String)
tokenGetTextMaybe c =
  tokenGetAntlrString c >>= \s ->
  fromAntlrStringToMaybeString (AntlrString s)

-- | Convert from an ANTLR string to a String.
-- | Note: the peekCStringLen function does not say what will happen if the
-- | c pointer is 0.
fromAntlrStringToString :: AntlrString -> IO String
fromAntlrStringToString (AntlrString x) = 
  {#get ANTLR3_STRING->chars#} x >>= \c ->
  {#get ANTLR3_STRING->len#} x >>= \l ->
  peekCStringLen ((castPtr c), (fromIntegral l)) >>= \s ->
  return s

-- | Obtain the token String for the specified token.
-- | >>> tokenGetText token
-- | Note: the peekCStringLen function does not say what will happen if the
-- | c pointer is 0.
tokenGetText :: Ptr (CommonToken) -> IO String
tokenGetText c =
  tokenGetAntlrString c >>= \s ->
  fromAntlrStringToString (AntlrString s)

-- | Obtain the token identifier for the specified token.
-- | >>> tokenGetType token
tokenGetType :: (Enum e) => Ptr (CommonToken) -> IO e
tokenGetType token = {#get ANTLR3_COMMON_TOKEN->type#} token >>= return . cToEnum

-- | Obtain the character position in the source code line of where the token
-- | was read, for non-imaginary tokens.
-- | >>> tokenGetCharPositionInLine token
tokenGetCharPositionInLine :: Ptr (CommonToken) -> IO Int
tokenGetCharPositionInLine token = {#get ANTLR3_COMMON_TOKEN->charPosition#} token >>= return . cIntConv

-- | Obtain the the source code line of where the token was read, for non-imaginary tokens.
-- | >>> tokenGetLine token
tokenGetLine :: Ptr (CommonToken) -> IO Int
tokenGetLine token = {#get ANTLR3_COMMON_TOKEN->line#} token >>= return . cIntConv
