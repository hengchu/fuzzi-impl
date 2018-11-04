module PatternQQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Syntax ()
import qualified Parser as P
import qualified PatternParser as PP

quoteCmd :: String -> Q Exp
quoteCmd input = do
  case P.parse P.parseCmd input of
    Left err -> fail err
    Right c -> dataToExpQ (const Nothing) c

cmd :: QuasiQuoter
cmd = QuasiQuoter { quoteExp = quoteCmd
                  , quotePat = fail
                  , quoteType = fail
                  , quoteDec = fail
                  }

quoteExprPattern :: String -> Q Exp
quoteExprPattern input = do
  case PP.parse PP.parseExprPattern input of
    Left err -> fail err
    Right ep -> dataToExpQ (const Nothing) ep

epat :: QuasiQuoter
epat = QuasiQuoter { quoteExp = quoteExprPattern
                   , quotePat = fail
                   , quoteType = fail
                   , quoteDec = fail
                   }

quoteCmdPattern :: String -> Q Exp
quoteCmdPattern input = do
  case PP.parse PP.parseCmdPattern input of
    Left err -> fail err
    Right cp -> dataToExpQ (const Nothing) cp

cpat :: QuasiQuoter
cpat = QuasiQuoter { quoteExp = quoteCmdPattern
                   , quotePat = fail
                   , quoteType = fail
                   , quoteDec = fail
                   }
