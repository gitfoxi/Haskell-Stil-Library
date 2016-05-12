
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Stil.ScanStructures
 ( ScanChain(..)
 , parseScanStructures
 , parseScanChain -- maybe don't export
 , parse
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

data ScanChain =
 ScanChain
 { scName :: ByteString
 , scLength :: Int
 , scScanIn :: ByteString
 , scScanOut :: ByteString
 , scScanInversion :: Bool
 , scScanCells :: [ScanCell]
 , scScanMasterClock :: [ByteString]
 }
 deriving (Show)

data ScanCell =
   ScanCell ByteString
 | Inverter
 deriving (Show)

blankScanChain = ScanChain "?" 0 "?" "?" False [] []

type Parser = ParsecT [Tok] ScanChain Identity

parse :: [Tok] -> Either ParseError [ScanChain]
parse = runParser parseScanStructures blankScanChain ""

-- TODO: parMap scanchains
parseScanStructures :: Parser [ScanChain]
parseScanStructures = do
  manyTill anyToken (ezToken (TIdent "ScanStructures"))
  lbrace
  scs <- many (try parseScanChain)
  rbrace
  return scs

-- TODO: simple token matchers
--       idents in any order or missing with default or unknown with warning
parseScanChain :: Parser ScanChain
parseScanChain = do
  ident "ScanChain"
  nm <- str
  putState blankScanChain { scName=nm }
  lbrace
  many (choice [
     scanLength
   , scanIn
   , scanOut
   , scanInversion 
   , scanMasterClock 
   , scanCells
   , unknownAttr
   ])

{-
  ident "ScanCells"
  scells <- many (str <|> bang)
  semi
  ident "ScanMasterClock"
  clocks <- many str
  semi
-}
  rbrace

  -- return $ ScanChain nm len si so sinv scells clocks
  getState

unknownAttr :: Parser ()
unknownAttr = do
 attr <- anyIdent
 warn ("ignoring unknown attr: " <> attr)
 many (dontMatch TSemi) -- TODO: Balanced braces
 semi
 return ()

warn = traceShowM

str = token show noPos match
 where
  match (TStr s) = Just s
  match _        = Nothing

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
 
scanCells :: Parser ()
scanCells = do
 ident "ScanCells"
 cs <- many ((ScanCell <$> str)
         <|> (Inverter <$ bang))
 semi
 modifyState (\sc -> sc {scScanCells=cs})

scanIn :: Parser ()
scanIn = do
 ident "ScanIn"
 si <- str
 semi
 modifyState (\sc -> sc {scScanIn=si})

scanOut :: Parser ()
scanOut = do
 ident "ScanOut"
 so <- str
 semi
 modifyState (\sc -> sc {scScanOut=so})

scanMasterClock :: Parser ()
scanMasterClock = do
 ident "ScanMasterClock"
 cs <- many str
 semi
 modifyState (\sc -> sc {scScanMasterClock=cs})

scanInversion :: Parser ()
scanInversion = do
 ident "ScanInversion"
 i <- int
 semi
 let inv = i == 1
 modifyState (\sc -> sc {scScanInversion=inv})

scanLength :: Parser ()
scanLength = do
 ident "ScanLength"
 len <- int
 semi
 modifyState (\sc -> sc {scLength=len})

int :: Parser Int
int = toInt <$> matchScientific
 where
  toInt = fromIntegral . fromJust . as
  as :: Scientific -> Maybe Int
  as = toBoundedInteger

matchScientific :: Parser Scientific
matchScientific = token show noPos m
 where
  m (TScientific n) = Just n
  m _               = Nothing

ident :: ByteString -> Parser ()
ident s = void $ ezToken (TIdent s)

lbrace = ezToken TLBrace
rbrace = ezToken TRBrace
semi   = ezToken TSemi
bang   = ezToken TBang
ezToken :: Tok -> Parser Tok
ezToken t = tokenPrim show (\s _ _ -> s) (\x -> maybe (x == t) t)

maybe :: Bool -> a -> Maybe a
maybe True x = Just x
maybe False _ = Nothing

noPos :: a -> SourcePos
noPos t = newPos "" 0 0
