{-# LANGUAGE OverloadedStrings #-}
{-#  #-}
-- | Generates persistent model and routes for
-- each tables of frontaccounting schema.
module Main where

import Prelude
import qualified Database.MySQL.Simple as SQL
import Data.List
import Data.List.Split(splitOn)
import Data.Char
import Data.Function
import Text.Printf


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
      \ ORDER BY table_name \
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


generateModel :: Table -> IO ()
generateModel Table{..} = do
  printf "%s sql=%s\n"
         (capitalize . camelCase . singularize . dropNonLetterPrefix $ tableName)
         tableName
  mapM_ go tableColumns
  putStrLn ""
  where go Column{..} = do
          printf "    %s %s %s sql=%s\n"
                 (camelCase columnName)
                 (htype columnType)
                 (if columnNullabe
                    then "Maybe" :: String
                    else ""
                 )
                 columnName


generateRoute table = do
  return ()

generateHandler table = do
  return ()
  

        



dropNonLetterPrefix :: String -> String
dropNonLetterPrefix [] = []
dropNonLetterPrefix (x:xs)
    | isLetter x = (x:xs)
    | otherwise = dropNonLetterPrefix xs

camelCase :: String -> String
camelCase [] = []
camelCase (xs) =  let
    words = splitOn "_"  xs
    uppers = concatMap capitalize words
    in uncapitalize uppers
    

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (x:xs) = toLower x : xs


-- remove trailing s at the moment
singularize = reverse . go . reverse where
  go [] = []
  go ('s':s) = s
  go s = s

htype :: String -> String
htype "varchar" = "String"
htype "bigint" = "Integer"
htype "longtext" = "String"
htype "datetime" = "Time.UTCTime"
htype "int" = "Int"
htype "decimal" = "Ratio"
htype "text" = "String"
htype "longblob" = "String"
htype "tinyint" = "Bool"
htype "smallint" = "Int"
htype "mediumtext" = "String"
htype "blob" = "String"
htype "tinytext" = "String"
htype "mediumint" = "String"
htype "float" = "Float"
htype "double" = "Double"
htype "date" = "Time.Day"
htype "timestamp" = "Time.UTCTime"
htype "char" = "String"
htype "tinyblob" = "String"
htype "varbinary" = "String"
-- htype "set" = "String"
-- htype "enum" = "String"
htype "time" = "Time.TimeOfDay"
htype s = "<-- Please edit me " ++  s ++ " -->"
