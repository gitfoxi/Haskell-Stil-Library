
import           Data.ByteString.Char8 as BS
import           Prelude hiding (lex)
-- import           Text.Parsec.Prim (runParser)

import Language.Stil.Lex
import qualified Language.Stil.ScanStructures as ScanStructures

main = do
 contents <- BS.getContents
 let rtokens = lex contents
     tokens =
      case rtokens of
        Left s -> error s
        Right ts -> ts
 -- mapM_ print tokens
 
     scanChains =
      case ScanStructures.parse tokens of
       Left e -> error (show e)
       Right a -> a
 print scanChains
