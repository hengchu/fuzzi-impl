module Main where

import Data.List

import qualified Data.ByteString.Lazy.Char8 as B8 (hPutStrLn)
import Data.Aeson hiding (Options)
import Data.Aeson.TH hiding (Options)
import qualified Data.Map as M
import SyntaxExt
import Shape
import Python
import Speculative.Sensitivity
import Composed
import qualified ParserExt as P
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import System.Console.GetOpt
import Text.PrettyPrint

data ExecMode = SensitivityCheck
              | TranspileOnly
              deriving (Show, Eq, Ord)

data Options = Options {
  optMode :: ExecMode
  , optExtFile :: String -- the path to the extension library file
  , optInputFile :: String -- use "stdin" for stdin
  , optOutputFile :: String -- use "-" for stdout
  , optDataFile :: String
  } deriving (Show, Eq, Ord)

data Output = Output {
  _output_sensitivities :: M.Map Var Float
  , _output_epsilon :: Float
  , _output_delta :: Float
  } deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 8} ''Output)

options :: [OptDescr (Options -> IO Options)]
options =
  [
    Option "s" ["sensitivity"]
      (NoArg
         (\opt -> return $ opt{optMode = SensitivityCheck}))
      "Sensitivity check",
    Option "t" ["transpile"]
      (ReqArg
        (\dt opt -> return $ opt{optDataFile = dt, optMode = TranspileOnly})
         "FILE")
      "JSON data file path",
    Option "I" ["file"]
      (ReqArg
        (\arg opt -> return $ opt{optExtFile=arg})
        "FILE")
      "Extension library path",
    Option "f" ["file"]
      (ReqArg
        (\arg opt -> return $ opt{optInputFile = arg})
        "FILE")
      "Input file path",
    Option "o" ["file"]
      (ReqArg
        (\arg opt -> return $ opt{optOutputFile = arg})
        "FILE")
      "Output file path",
    Option "h" ["help"]
      (NoArg
        (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitWith ExitSuccess))
      "Show help"
  ]

startOptions :: Options
startOptions = Options SensitivityCheck "" "stdin" "-" ""

main :: IO ()
main = do
  args <- getArgs

  let (actions, nonOptions, errors) = getOpt RequireOrder options args

  opts <- foldl (>>=) (return startOptions) actions

  when (not . null $ nonOptions) $ do
    hPutStrLn stderr $ "unknown options: " ++ (intercalate ", " nonOptions)
    exitFailure

  when (not . null $ errors) $ do
    hPutStrLn stderr $ "error in parsing commands: " ++ (intercalate ", " errors)
    exitFailure

  let inputFile = optInputFile opts
  let jsonPath = optDataFile opts

  inputFd <- if inputFile == "stdin" then return stdin else openFile inputFile ReadMode

  inputContent <- hGetContents inputFd

  extLib <-
    case optExtFile opts of
      [] -> return M.empty
      path -> do
        extFileFd <- openFile path ReadMode
        extFileContent <- hGetContents extFileFd
        case P.parse P.parseExtLib extFileContent of
          Left err -> do
            hPutStrLn stderr err
            exitFailure
          Right (Prog _ cmd) ->
            case getExtensionLibrary cmd of
              Left errs -> do
                hPutStrLn stderr $ show errs
                exitFailure
              Right extLib -> return extLib

  ast@(Prog decls cmd) <-
    case P.parse P.parseProg inputContent of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right ast -> return ast

  let shapeCxt = extractShapeCxt ast
  let sensCxt = extractSensCxt ast

  cmd' <-
    case desugarExtensions' extLib cmd of
      Left err -> do
        hPutStrLn stderr $ show err
        exitFailure
      Right cmd' -> return cmd'

  case optMode opts of
    SensitivityCheck ->
      case runComposedChecker shapeCxt sensCxt cmd' of
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right (SensCxt cxt _ _, eps, delta) -> do
          let output = Output cxt eps delta
          B8.hPutStrLn stdout $ encode output
          exitSuccess
    TranspileOnly ->
      case runPythonify jsonPath decls cmd' of
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right code -> do
          let output = render code
          hPutStrLn stdout output
          exitSuccess
