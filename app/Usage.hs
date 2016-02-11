{-# LANGUAGE QuasiQuotes #-}
module Usage (progUsage) where

import System.Environment (getArgs)
import System.Console.Docopt

progUsage :: Docopt
progUsage = [docopt|
Huntex.

Usage:
    huntex FILE [ -a ]
    huntex --help | -h
    huntex --version

Options:
    -h, --help      Show this screen.
    -a, --ast       Dump AST
    --version       Show version.

Arguments
    FILE            File to be 'huntex'ed
|]
