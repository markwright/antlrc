#include "antlr3lexer.h"

/* The function prototypes are in Lexer.chs */

ANTLR3_COMMON_TOKEN *LT(ANTLR3_INPUT_STREAM *input, int lti)
{
  return input->_LT(input, (ANTLR3_INT32)lti);
}

ANTLR3_STRING *tokenGetAntlrString(ANTLR3_COMMON_TOKEN *token)
{
  return token->getText(token);
}
