{-# LANGUAGE OverloadedStrings #-}
-- | Generates persistent model and routes for
-- each tables of frontaccounting schema.
module Main where

import Prelude
import Control.Monad
import qualified Database.MySQL.Simple as SQL
import Data.List
import Data.List.Split(splitOn)
import Data.Char
import Data.Function
import Text.Printf
import Data.String (fromString)

import System.Environment (getArgs)
import System.IO (openFile, Handle, IOMode(..), hClose, hPrint, hPutStrLn)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data Table = Table
  { tableName :: String
  , sqlName :: String
  , tableColumns :: [Column]
  } deriving Show


data Column = Column
  { columnName :: String
  , columnType :: String
  , columnNullabe :: Bool
  , columnIsPrimary :: Bool
  } deriving (Show, Read, Eq, Ord)


data FilterMode = All | OnlyFA | ExcludeFA | Fames deriving (Eq, Read, Show)

-- loadSchema :: SQL IO [Table]
loadSchema mode connectInfo database = do
  let
    tableQuery = case mode of
      All    -> ""
      OnlyFA -> " AND " ++ faTables
      ExcludeFA ->
        " AND NOT (" ++ faTables ++ ") AND table_name not like 'fames_%' "
      Fames -> "AND table_name like 'fames_%'"
     where
      faTables = "table_name like '0_%' and table_name != '0_item_requests' "
  conn <- SQL.connect connectInfo
  rows <- SQL.query
    conn
    ( fromString
    $ "\
      \ SELECT table_name \
      \      , column_name \
      \      , data_type \
      \      , is_nullable \
      \      , column_key \
      \ FROM information_schema.columns \
      \ WHERE table_schema = ? "
    ++ (tableQuery :: String)
    ++ " \
      \ AND table_name not like '% %' \
      \ AND column_name not like '% %' \
      \ ORDER BY table_name, ordinal_position \
      \"
    )
    [database :: String]
  let groups =
        groupBy ((==) `on` fst) [ (t, (c, d, n, p)) | (t, c, d, n, p) <- rows ]
  return $ map makeTable groups
 where
  makeTable rows =
    let name         = (fst . head $ rows)
        tableName    = dropNonLetterPrefix name
        sqlName      = {-database ++ "." ++ -} name
        tableColumns = (map makeColumn rows)
    in  Table {..}
  makeColumn (_, (name, dtype, nullable, primary)) =
    let nullable_ = nullable == ("YES" :: String)
    in Column name
           dtype
           nullable_
           (primary == ("PRI" :: String) && not nullable_)

main :: IO ()
main = do
  args <- getArgs
  let (db, module_, mode) = case args of
        ["fax"] -> ("fa", "FAX", ExcludeFA)
        ["fames"] -> ("fa", "FAMES", Fames)
        _       -> ("fa", "FA", OnlyFA)
      connectInfo         = SQL.defaultConnectInfo
         { SQL.connectHost = "127.0.0.1"
         , SQL.connectUser = "root"
         , SQL.connectPassword = "stag"
         , SQL.connectDatabase = db
         }
      moduleLower         = map toLower module_
  tables   <- loadSchema mode connectInfo db


  outModel <- openFile (printf "config/%s-models" moduleLower) WriteMode 
  hPutStrLn outModel "-- Warning ! This code has been generated !"
  hPutStrLn outModel "-- Model"
  mapM_ (generateModel outModel) tables
  hClose outModel

  outRoute <- openFile (printf "config/%s-routes" moduleLower) WriteMode
  hPutStrLn outRoute "-- Warning ! This code has been generated !}"
  hPutStrLn outRoute "\n-- Route"
  hPutStrLn outRoute
    $  "/db/"
    ++ moduleLower
    ++ " "
    ++ module_
    ++ "'R !db !"
    ++ moduleLower
    ++ ":"
  mapM_ (generateRoute outRoute module_) tables
  hClose outRoute


  let handlerDir = "Handler" </> module_
  createDirectoryIfMissing True handlerDir
  outHandler <- openFile (handlerDir </> "Def.hs") WriteMode 
  hPutStrLn outHandler "-- Warning ! This code has been generated !"
  hPutStrLn outHandler
    $  "-- Handler\n\
\module Handler."
    ++ module_
    ++ ".Def where\n\
\import Import\n\
\import "
    ++ module_
    ++ "\n\
\\n"

  mapM_ (generateHandler outHandler module_) tables
  hClose outHandler


generateModel :: Handle -> Table -> IO ()
generateModel out Table {..} = do
  hPrintf out "%s sql=%s\n" (model $ tableName) sqlName
  -- generate primary keys if composite
  let composites = filter ((==True) . columnIsPrimary) tableColumns
      -- fieldName' = show
      fieldName Column{..} = if (columnIsPrimary  || columnName == "id") && length composites == 1
                             then "Id"
                             else if columnName == "id"
                                  then "id_"
                                  else sanitize $ camelCase columnName
      go :: Column -> IO ()
      go col@(Column {..}) = do
        hPrintf out
          "    %s %s %s sql=%s\n"
          (fieldName col)
          (htype columnType)
          (if columnNullabe then "Maybe" :: String else "")
          columnName
  mapM go tableColumns
  when (length composites > 1)
       (  hPutStrLn out
       $  "    Primary "
       ++ unwords (map fieldName composites)
       )

  hPutStrLn out ""


generateRoute :: Handle -> String -> Table -> IO ()
generateRoute out module_ Table {..} = do
  hPrintf out "  /%s %s GET\n" tableName (handler module_ tableName)

handler :: String -> String -> String
handler module_ s = printf "%s%sR" module_ (capitalize $ camelCase s)

model :: String -> String
model = capitalize . camelCase . singularize .sanitize

generateHandler :: Handle -> String -> Table -> IO ()
generateHandler out module_ Table {..} = do
  let handlerName = "get" ++ handler module_ tableName
  hPrintf out
    "\
\%s :: Handler Html \n\
\%s = entityTableHandler (%s'R %s) ([] :: [Filter %s.%s]) \n\
\\n"
    handlerName
    handlerName
    module_
    (handler module_ tableName)
    module_
    (model tableName)








dropNonLetterPrefix :: String -> String
dropNonLetterPrefix [] = []
dropNonLetterPrefix (x:xs)
  | isLetter x
  = (x : xs)
  | otherwise
  = dropNonLetterPrefix xs

camelCase :: String -> String
camelCase [] = []
camelCase (xs) =
  let words  = splitOn "_" xs
      uppers = concatMap capitalize words
  in  uncapitalize uppers


capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

uncapitalize :: String -> String
uncapitalize []     = []
uncapitalize (x:xs) = toLower x : xs


-- remove trailing s at the moment
singularize = reverse . go . reverse
 where
  go []                = []
  go s@('s'    :'s':_) = s
  go (  's':'e':'i':s) = 'y' : s
  go (  's'        :s) = s
  go s                 = s

-- remove  invalid character
sanitize :: String -> String
sanitize = map go where
  go c | isPunctuation c = '_'
       | otherwise = c

htype :: String -> String
htype "varchar"    = "Text"
htype "bigint"     = "Int"
htype "longtext"   = "Text"
htype "datetime"   = "UTCTime"
htype "int"        = "Int"
htype "decimal"    = "Rational"
htype "text"       = "Text"
htype "longblob"   = "ByteString"
htype "tinyint"    = "Bool"
htype "smallint"   = "Int"
htype "mediumtext" = "Text"
htype "blob"       = "ByteString"
htype "tinytext"   = "Text"
htype "mediumint"  = "Int"
htype "float"      = "Double"
htype "double"     = "Double"
htype "date"       = "Day"
htype "timestamp"  = "UTCTime"
htype "char"       = "Text"
htype "tinyblob"   = "ByteString"
htype "varbinary"  = "ByteString"
-- htype "set" = "String"
-- htype "enum" = "String"
htype "time"       = "TimeOfDay"
htype s            = "<-- Please edit me " ++ s ++ " -->"
