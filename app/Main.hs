module Main where

import Data.List

import Syntax
import ShapeChecker
import SensitivityChecker
import Python
import Expand
import qualified Parser as P
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import System.Console.GetOpt
import Text.PrettyPrint

data ExecMode = ShapeCheckOnly
              | SensitivityCheck
              | TranspileOnly
              deriving (Show, Eq, Ord)

data Options = Options {
  optMode :: ExecMode
  , optInputFile :: String -- use "stdin" for stdin
  , optOutputFile :: String -- use "-" for stdout
  , optDataFile :: String
  } deriving (Show, Eq, Ord)

options :: [OptDescr (Options -> IO Options)]
options =
  [
    Option "S" ["shape"]
      (NoArg
         (\opt -> return $ opt{optMode = ShapeCheckOnly}))
      "Shape check only",
    Option "s" ["sensitivity"]
      (NoArg
         (\opt -> return $ opt{optMode = SensitivityCheck}))
      "Sensitivity check",
    Option "t" ["transpile"]
      (ReqArg
        (\dt opt -> return $ opt{optDataFile = dt})
         "FILE")
      "JSON data file path",
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
startOptions = Options SensitivityCheck "stdin" "-" ""

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
  let outputFile = optOutputFile opts
  let jsonPath = optDataFile opts

  inputFd <- if inputFile == "stdin" then return stdin else openFile inputFile ReadMode

  inputContent <- hGetContents inputFd

  ast <-
    case P.parse P.parseProg inputContent of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right ast -> return ast

  let ast' = ast{getCmd = desugarFix (getCmd ast) fuzziExpansionRules}

  case optMode opts of
    ShapeCheckOnly -> do
      case runShapeChecker ast' of
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right _ -> do
          hPutStrLn stdout $ "program passed shape checker"
          exitSuccess
    SensitivityCheck -> do
      case runSensitivityChecker ast' 100000 of
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right ctx ->
          hPutStrLn stdout $ show ctx
    TranspileOnly -> do
      case runTranspiler jsonPath ast' of
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right pythonCode -> do
          outputFd <- if outputFile == "-" then return stdout else openFile outputFile WriteMode
          hPutStrLn outputFd $ render pythonCode
          exitSuccess
