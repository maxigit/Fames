{-# LANGUAGE OverloadedStrings #-}
-- | Generates persistent model and routes for
-- each tables of frontaccounting schema.
module Main where

import Prelude
import qualified Database.MySQL.Simple as SQL
import Data.List
import Data.Function


data Table = Table 
  { tableName :: String
  , tableColumns :: [Column]
  } deriving Show


data Column = Column
  { columnName :: String
  , columnType :: String
  , columnNullabe :: Bool
  } deriving (Show, Read, Eq, Ord)



-- loadSchema :: SQL IO [Table]
loadSchema connectInfo database = do
  conn <- SQL.connect connectInfo
  rows <- SQL.query  conn "\
      \ SELECT table_name \
      \      , column_name \
      \      , data_type \
      \      , is_nullable \
      \ FROM information_schema.columns \
      \ WHERE table_schema = ? \
      \ AND table_name like '0_%' \
      \ AND table_name not like '% %' \
      \ AND column_name not like '% %' \
      \ ORDER BY column_name \
      \" [database :: String]
  let groups = groupBy ((==) `on` fst) [(t, (c,d,n)) | (t,c,d,n) <- rows]
  return $ map makeTable groups

  where makeTable rows = Table (fst.head $ rows) (map makeColumn rows)
        makeColumn (_, (name, dtype, nullable)) = Column name dtype (nullable == ("YES" :: String ))

main :: IO ()
main = do
  let connectInfo = SQL.defaultConnectInfo
         { SQL.connectHost = "172.17.0.3"
         , SQL.connectUser = "test"
         , SQL.connectPassword = "test"
         , SQL.connectDatabase = db
         }
      db = "Fames_test"
  tables <- loadSchema connectInfo db
  print "#Model"
  mapM_ generateModel tables
  print "#Route"
  mapM_ generateRoute tables
  print "#Handler"
  mapM_ generateHandler tables


generateModel table = do
  print table


generateRoute table = do
  print table

generateHandler table = do
  print table
  

        


