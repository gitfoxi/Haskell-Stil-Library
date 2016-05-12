
{-# LANGUAGE OverloadedStrings #-}

module Language.Stil.Lex
 ( Tok(..)
 , Language.Stil.Lex.lex
 ) where

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.Scientific
import           Prelude hiding (takeWhile)

data Tok =
    TSemi
  | TComma
  | TLBrace
  | TRBrace
  | TStr ByteString
  | TIdent ByteString
  | TScientific Scientific
  | TDouble Double
  | TRArrow
  | TEquals
  | TPlus
  | TMinus
  | TComment ByteString
  | TBang
  | TColon
  | TPound
  deriving (Show, Eq)

lex :: BS.ByteString -> Either String [Tok]
lex bs = parseOnly tokens bs

tokens :: Parser [Tok]
tokens = spaces *> many' token

token :: Parser Tok
token = lexeme $ choice
 [ semi
 , comma
 , anstr
 , lbrace
 , rbrace
 , rarrow
 , equals
 , plus
 , minus
 , num
 , qstr
 , sqstr
 , comment
 , ident
 , bang
 , colon
 , pound
 ]

ident :: Parser Tok
ident = TIdent <$> takeWhile1 identChar

identChar = inClass "0-9a-zA-Z_\\[\\]\\"

comment :: Parser Tok
comment = TComment . BS.pack <$> (string "//" *> manyTill anyChar endOfLineOrInput )

endOfLineOrInput :: Parser ()
endOfLineOrInput = (void $ char '\n') <|> endOfInput

-- {* anything at all *}
anstr :: Parser Tok
anstr = TStr . BS.pack <$> (string "{*" *> manyTill anyChar (string "*}") )

sqstr :: Parser Tok
sqstr = TStr . BS.concat <$> (squote *> many' (escaped <|> notSquote) <* squote)

qstr :: Parser Tok
qstr = TStr . BS.concat <$> (dquote *> many' (escaped <|> notDquote) <* dquote)

semi :: Parser Tok
semi = char ';' *> return TSemi

comma :: Parser Tok
comma = char ',' *> return TComma

lbrace :: Parser Tok
lbrace = char '{' *> return TLBrace

rbrace :: Parser Tok
rbrace = char '}' *> return TRBrace

pound :: Parser Tok
pound = char '#' *> return TPound

colon :: Parser Tok
colon = char ':' *> return TColon

equals :: Parser Tok
equals = char '=' *> return TEquals

plus :: Parser Tok
plus = char '+' *> return TPlus

minus :: Parser Tok
minus = char '-' *> return TMinus

bang :: Parser Tok
bang = char '!' *> return TBang

rarrow :: Parser Tok
rarrow = string "->" *> return TRArrow

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

spaces :: Parser ()
spaces = void $ takeWhile isSpace

squote :: Parser ()
squote = void $ char '\''

notSquote :: Parser ByteString
notSquote = takeWhile1 (\x -> x /= '\'' && x /= '\\')

dquote :: Parser ()
dquote = void $ char '"'

notDquote :: Parser ByteString
notDquote = takeWhile1 (\x -> x /= '"' && x /= '\\')

escaped :: Parser ByteString
escaped = BS.singleton <$> (char '\\' *> anyChar)

num :: Parser Tok
num = TScientific <$> A.scientific
