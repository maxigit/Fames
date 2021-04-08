module Main where

import DPDLib
import Data.Text(pack)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [user, password] -> dumpFromWebsite (pack user) (pack password)
    _ -> error "Please run with USER PASSWORD"
