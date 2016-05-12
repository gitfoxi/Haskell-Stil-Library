
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
import Language.Stil.Prim

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

  rbrace

  getState

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

