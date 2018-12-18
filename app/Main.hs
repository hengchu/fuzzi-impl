module Main where

import Data.List

import Control.Lens
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B8 (hPutStrLn)
import Data.Aeson hiding (Options)
import Data.Aeson.TH hiding (Options)
import qualified Text.Tabular as TT
import qualified Text.Tabular.AsciiArt as TTA
import qualified Data.Map as M
import SyntaxExt
import Shape
import Speculative.Sensitivity
import Composed
import qualified ParserExt as P
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
  , optExtFile :: String -- the path to the extension library file
  , optInputFile :: String -- use "stdin" for stdin
  , optOutputFile :: String -- use "-" for stdout
  , optDataFile :: String
  , optDepth :: Int
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
    Option "d" ["depth"]
      (ReqArg
        (\arg opt -> return $ opt{optDepth = read arg})
        "INT")
      "Search depth",
    Option "h" ["help"]
      (NoArg
        (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitWith ExitSuccess))
      "Show help"
  ]

startOptions :: Options
startOptions = Options SensitivityCheck "" "stdin" "-" "" 100

{-
renderContext :: Float -> SContext -> String
renderContext eps (SContext sctx) =
  let epsRow = TT.row "epsilon" [show eps]
  in TTA.render id id (TTA.padLeft 10) $
       (M.foldrWithKey
         (\x s acc ->
             acc
             TT.+----+ (TT.row x [show s]))
         (TT.empty TT.^..^ TT.col "value" []
          TT.+====+ epsRow)
         sctx)
-}

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
  {-
  let outputFile = optOutputFile opts
  let jsonPath = optDataFile opts
  let depth = optDepth opts
  -}

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

  ast@(Prog _ cmd) <-
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

  case runComposedChecker shapeCxt sensCxt cmd' of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right (SensCxt cxt _ _, eps, delta) -> do
      let output = Output cxt eps delta
      B8.hPutStrLn stdout $ encode output
      exitSuccess

  {-
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
      case runSensitivityChecker ast' depth of
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right ctx ->
          mapM_ (hPutStrLn stdout) (map (uncurry renderContext) ctx)
    TranspileOnly -> do
      case runTranspiler jsonPath ast' of
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right pythonCode -> do
          outputFd <- if outputFile == "-" then return stdout else openFile outputFile WriteMode
          hPutStrLn outputFd $ render pythonCode
          exitSuccess
-}
