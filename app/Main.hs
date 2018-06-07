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
import Interp
import Python
import System.Exit
import Text.PrettyPrint
import Typechecker.Sensitivity
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data CLIOptions = CLIOptions {
  optFile :: String
  , optTypecheck :: Bool
  , optPretty :: Bool
  , optInterp :: String
  , optTranspile :: Bool
  }

instance Options CLIOptions where
  defineOptions = CLIOptions
    <$> simpleOption "file" "stdin"
                     "The input file"
    <*> simpleOption "tc" False
                     "Run sensitivity checker only?"
    <*> simpleOption "pretty" False
                     "Pretty print instead?"
    <*> simpleOption "interp" ""
                     "Run the interpreter instead?"
    <*> simpleOption "transpile" True
                     "Transpile to python"

main :: IO ()
main = runCommand $ \opts _ -> do
  let file = optFile opts
  progText <- if file == "stdin"
                then hGetContents stdin
                else readFile file
  let ast = parseProg . alexScanTokens $ progText

  when (optTranspile opts) $ do
    putStr $ transpile undefined (desugarAll ast)
    exitWith ExitSuccess

  when (optPretty opts) $ do
    putStr . render . prettyCmd . desugarAll $ ast
    exitWith ExitSuccess

  when (optTypecheck opts && length (optInterp opts) == 0) $ do
    let result = checkCmd ast
    case result of
      Left  errs      -> forM_ errs putStrLn
      Right (ctx, ep) -> do
        putStrLn $ "Epsilon = " ++ show ep
        forM_ (M.toList ctx) $ \(x, s) -> do
          putStrLn $ x ++ " : " ++ show s
    exitWith ExitSuccess

  when ((length $ optInterp opts) > 0) $ do
    fileContent <- BS.readFile (optInterp opts)
    case eitherDecodeStrict fileContent of
      Left err  -> putStrLn err
      Right mem -> do
        let mem' = interp (desugarAll ast) (getJsonMemory mem)
        BSL.putStrLn . encode . JsonMemory $ mem'
