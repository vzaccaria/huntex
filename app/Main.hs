{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Base.Parser
import Data.Text (unlines, pack, unpack)
import qualified Data.Text.IO as T

main :: IO ()
main = do {
  example <- readFile "./tests/2016-ICWE.tex";
  case parseLaTeX (pack example) of
    Left err -> print err
    Right l  -> do
      print l;
      let t = render l;
      print $ example == (unpack t);
      putStrLn "All done."
  }
