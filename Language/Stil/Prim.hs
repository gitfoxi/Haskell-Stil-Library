

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing helpers

module Language.Stil.Prim (
   unknownAttr
 , warn
 , str
 , anyIdent
 , dontMatch
 , int
 , matchScientific
 , ident
 , lbrace
 , rbrace
 , semi
 , bang
 , ezToken

 ) where

import           Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.Functor.Identity
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Scientific
import           Debug.Trace
import           Prelude hiding (maybe)
import           Text.Parsec.Combinator
import           Text.Parsec.Pos
import           Text.Parsec.Prim hiding (parse)
import           Text.Parsec (ParseError(..))

import Language.Stil.Lex (Tok(..))

unknownAttr :: Parsec [Tok] u ()
unknownAttr = do
 attr <- anyIdent
 warn ("ignoring unknown attr: " <> attr)
 many (dontMatch TSemi) -- TODO: Balanced braces end statement instead of semicolon
 semi
 return ()

warn = traceShowM

str ::  Parsec [Tok] u ByteString
str = token show noPos match
 where
  match (TStr s) = Just s
  match _        = Nothing

anyIdent ::  Parsec [Tok] u ByteString
anyIdent = token show noPos match
 where
  match (TIdent i) = Just i
  match _          = Nothing

-- | Match any token but m
dontMatch m = token show noPos match
 where
  match x
   | x == m    = Nothing
   | otherwise = Just x
 
int :: (Stream s Identity Tok) => Parsec s u Int
int = toInt <$> matchScientific
 where
  toInt = fromIntegral . fromJust . as
  as :: Scientific -> Maybe Int
  as = toBoundedInteger

matchScientific :: (Stream s Identity Tok) => Parsec s u Scientific
matchScientific = token show noPos m
 where
  m (TScientific n) = Just n
  m _               = Nothing

ident :: (Stream s m Tok) => ByteString -> ParsecT s u m ()
ident s = void $ itok
 where 
  itok :: Parser Tok
  itok = ezToken (TIdent s)

lbrace :: Parser Tok
lbrace = ezToken TLBrace

rbrace :: Parser Tok
rbrace = ezToken TRBrace

semi :: Parser Tok
semi   = ezToken TSemi

bang :: Parser Tok
bang   = ezToken TBang

type Parser a = forall s u m. (Eq a, Show a, Stream s m a, Monad m) => ParsecT s u m a

-- ezToken :: (Eq a, Show a, Stream s m a) => a -> ParsecT s u m a
ezToken :: a -> Parser a
ezToken t = tokenPrim show (\s _ _ -> s) (\x -> maybe (x == t) t)

maybe :: Bool -> a -> Maybe a
maybe True x = Just x
maybe False _ = Nothing

-- TODO: track SourcePos instead of punting like this
noPos :: a -> SourcePos
noPos t = newPos "" 0 0
