{-# LANGUAGE OverloadedStrings #-}
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

  where makeTable rows = Table (dropNonLetterPrefix . fst . head $ rows) (map makeColumn rows)
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
  putStrLn "-- Model"
  mapM_ generateModel tables
  putStrLn "\n-- Route"
  mapM_ generateRoute tables
  putStrLn "\n-- Handler\n\
\module Handler.FA.Def where\n\
\import Import\n\
\import FA\n\
\\n"

  mapM_ generateHandler tables


generateModel :: Table -> IO ()
generateModel Table{..} = do
  printf "%s sql=0_%s\n"
         (model $ tableName)
         tableName
  mapM_ go tableColumns
  putStrLn ""
  where go Column{..} | map toLower columnName == "id"
                        || map toLower columnName == tableName ++ "_id"
                                                 = printf "    Id sql=%s\n" columnName
                      | True = do
          printf "    %s %s %s sql=%s\n"
                 (camelCase columnName)
                 (htype columnType)
                 (if columnNullabe
                    then "Maybe" :: String
                    else ""
                 )
                 columnName


generateRoute :: Table -> IO ()
generateRoute Table{..} = do
  printf "/fa/%s %s GET\n"
         tableName
         (handler tableName)

handler :: String -> String
handler s = printf "FA%sR" (capitalize  $ camelCase s)

model :: String -> String
model = capitalize . camelCase . singularize

generateHandler :: Table -> IO ()
generateHandler Table{..} = do
  let handlerName = "get" ++ handler tableName
  printf "\
\%s :: Handler Html \n\
\%s = do \n\
\  entities <- runDB $ selectList [] []\n\
\  let typed = entities :: [Entity FA.%s]\n\
\  defaultLayout $ toWidget (entitiesToTable getDBName entities)\n\
\\n"
         handlerName
         handlerName
         (model tableName)


  

        



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
htype "varchar" = "Text"
htype "bigint" = "Int"
htype "longtext" = "Text"
htype "datetime" = "UTCTime"
htype "int" = "Int"
htype "decimal" = "Rational"
htype "text" = "Text"
htype "longblob" = "ByteString"
htype "tinyint" = "Bool"
htype "smallint" = "Int"
htype "mediumtext" = "Text"
htype "blob" = "ByteString"
htype "tinytext" = "Text"
htype "mediumint" = "Int"
htype "float" = "Double"
htype "double" = "Double"
htype "date" = "Day"
htype "timestamp" = "UTCTime"
htype "char" = "Text"
htype "tinyblob" = "ByteString"
htype "varbinary" = "ByteString"
-- htype "set" = "String"
-- htype "enum" = "String"
htype "time" = "TimeOfDay"
htype s = "<-- Please edit me " ++  s ++ " -->"
