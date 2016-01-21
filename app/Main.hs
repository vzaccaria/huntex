{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Base.Syntax
import Data.Text (unlines, pack, unpack)
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Console.Docopt
import Control.Monad (when)


_sp:: Int -> String
_sp n = foldl1 (++) spaces where
  spaces = replicate n (" ")

_er :: String -> String
_er [] = ""
_er (x:s)
  | (x == '\n') = '\n' : (_er s)
  | otherwise   = ' ' : (_er s)

_eraseTex :: LaTeX -> String
_eraseTex p = _er ((unpack . render) p)

_eraseEnvProlog :: String -> String
_eraseEnvProlog s = (_er "\\begin{") ++ (_er s) ++ (_er "}")

_eraseEnvEpilog :: String -> String
_eraseEnvEpilog s = (_er "\\end{") ++ (_er s) ++ (_er "}")

_eraseSimpleCommand :: String -> String -> String
_eraseSimpleCommand name content = (_er "\\") ++ (_er name) ++ (_er "{") ++ content ++ (_er "}")



detex :: LaTeX -> String
detex (TeXSeq l1 l2) = (detex l1) ++ (detex l2)
detex (TeXRaw t)        = (unpack t)

detex p@(TeXComm "emph" [FixArg (TeXRaw content)])   = _eraseSimpleCommand "emph" (unpack content)
detex p@(TeXComm _ _)   = (_eraseTex p)

detex p@(TeXComment _)  = (_eraseTex p)
detex p@(TeXEnv "thebibliography" args content) = (_eraseTex p)
detex p@(TeXEnv "document" args content) = (_eraseEnvProlog "document") ++ (detex content) ++ (_eraseEnvEpilog "document")
detex p@(TeXEnv "abstract" args content) = (_eraseEnvProlog "abstract") ++ (detex content) ++ (_eraseEnvEpilog "abstract")
detex p@(TeXEnv _ _ _ ) = (_eraseTex p)
detex p = (_eraseTex p)

progUsage :: Docopt
progUsage = [docoptFile|./docs/USAGE.md|]

_rArg :: String -> Docopt -> Arguments -> (IO String)
_rArg name doc opts = getArgOrExitWith doc opts (argument name)

_lOpt :: String -> Arguments -> Bool
_lOpt name opts = (isPresent opts (longOption name))

detexFile :: String -> IO ()
detexFile name = do {
  example <- readFile name;
  case parseLaTeX (pack example) of
    Left err -> print err
    Right l  -> do
      putStrLn (detex l)
  }

dispatchOptions :: Docopt -> Arguments -> IO ()
dispatchOptions doc opts = do {
  let options = (_lOpt "version" opts, _lOpt "help" opts) in
     case options of
        (True, _) -> putStrLn "V1.0";
        (_, True) -> exitWithUsage doc
        _ -> do {
          file <- _rArg "FILE" doc opts;
          detexFile file
        }
  }

main :: IO ()
main = do {
  opts <- parseArgsOrExit progUsage =<< getArgs;
  dispatchOptions progUsage opts
  }
