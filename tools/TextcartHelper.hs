{-# LANGUAGE OverloadedStrings #-}
-- | Analyse the stock and generate 
-- a vim script to generate abbreviatin to stock_id
-- to be used in a textcart.
module Main where


import qualified Data.Map as Map
import qualified Database.MySQL.Simple as SQL
import Text.Printf
import System.Environment (getArgs)
import Control.Monad
import Debug.Trace


readStock :: SQL.ConnectInfo ->  IO [(String, String)]
readStock connectInfo = do
  let query = "SELECT DISTINCT LEFT(stock_id,3) base, SUBSTRING(stock_id, 4,5) style FROM 0_stock_master WHERE inactive = 0 and stock_id like 'M___-___%'";
  conn <- SQL.connect connectInfo
  rows <- SQL.query_ conn query
  return rows


readStock' :: SQL.ConnectInfo ->  IO [(String, String)]
readStock' connectInfo = do
  let query = "SELECT DISTINCT LEFT(stock_id,5) base, SUBSTRING(stock_id, 6,3) style FROM 0_stock_master WHERE inactive = 0 and stock_id like 'M___-___%'";
  conn <- SQL.connect connectInfo
  rows <- SQL.query_ conn query
  return rows
  
main :: IO ()
main = do
  (port:password:_) <- getArgs
  let connectInfo = SQL.defaultConnectInfo
         { SQL.connectHost = "127.0.0.1"
         , SQL.connectUser = "root"
         , SQL.connectPassword = password
         , SQL.connectPort = read port
         , SQL.connectDatabase = "fa"
         }


  putStrLn "%s/.*/\\U&/"
  go readStock connectInfo
  go readStock' connectInfo

go :: (SQL.ConnectInfo -> IO [(String, String)]) -> SQL.ConnectInfo -> IO ()
go reader connectInfo = do
  stocks <- reader connectInfo
  let groups = Map.fromListWith (++) [(style, [base]) | (base, style) <- stocks ]
      uniques = filter (\(_, bs) -> length bs == 1) (Map.toList groups)
      duplicates = filter (\(_, bs) -> length bs > 1) (Map.toList groups)


  forM_ uniques (\(style, [base]) -> printf "%%s/^%s/:%s&-#/ie\n" style base)
  





