module Main where

import Control.Monad
import Data.Map as M
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Lexer
import Options
import Parser
import Pretty
import Syntax
import System.Exit
import Text.PrettyPrint
import Typechecker.Sensitivity

data CLIOptions = CLIOptions {
  optFile :: String
  , optTypecheck :: Bool
  , optPretty :: Bool
  }

instance Options CLIOptions where
  defineOptions = CLIOptions
    <$> simpleOption "file" "stdin"
                     "The input file"
    <*> simpleOption "tc" True
                     "Run typechecker?"
    <*> simpleOption "pretty" False
                     "Pretty print instead?"

main :: IO ()
main = runCommand $ \opts _ -> do
  let file = optFile opts
  progText <- if file == "stdin"
                then hGetContents stdin
                else readFile file
  let ast = parseProg . alexScanTokens $ progText

  when (optPretty opts) $ do
    putStr . render . prettyCmd . desugar $ ast
    exitWith ExitSuccess

  when (optTypecheck opts) $ do
    let result = checkCmd ast
    case result of
      Left  errs      -> forM_ errs putStrLn
      Right (ctx, ep) -> do
        putStrLn $ "Epsilon = " ++ show ep
        forM_ (M.toList ctx) $ \(x, s) -> do
          putStrLn $ x ++ " : " ++ show s
