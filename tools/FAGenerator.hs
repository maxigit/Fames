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
  , columnNullable :: Bool
  , columnIsPrimary :: Bool
  } deriving (Show, Read, Eq, Ord)


data FilterMode = All | OnlyFA | ExcludeFA | Fames | DC deriving (Eq, Read, Show)

-- loadSchema :: SQL IO [Table]
loadSchema mode connectInfo database = do
  let
    tableQuery = case mode of
      All    -> ""
      OnlyFA -> " AND " ++ faTables
      ExcludeFA ->
        " AND NOT (" ++ faTables ++ ") AND table_name not like 'fames_%' AND table_name not like 'dcx_%'"
      Fames -> "AND table_name like 'fames_%'"
      DC -> ""
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
        sqlName      = (if mode == DC
                       then ("dcx_" ++)
                       else id
                       ) {-database ++ "." ++ -} name
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
        -- For the federated table, we need to get the information
        -- from the federated table but not the view, as the view
        -- lacks some columns information (like primary)
        ["dc"] -> ("commerceX", "DC", DC)
        _       -> ("fa", "FA", OnlyFA)
      connectInfo         = SQL.defaultConnectInfo
         { SQL.connectHost = "127.0.0.1"
         , SQL.connectUser = "root"
         , SQL.connectPassword = "stag"
         , SQL.connectPort = 3308
         , SQL.connectDatabase = db
         }
      moduleLower         = map toLower module_
  tables   <- loadSchema mode connectInfo db


  outModel <- openFile (printf "config/%s-models" moduleLower) WriteMode 
  hPutStrLn outModel "-- Warning ! This code has been generated !"
  hPutStrLn outModel "-- Model"
  mapM_ (generateModel mode outModel) tables
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
  zipWithM (generateRoute outRoute module_) tables ("!group=Tables":repeat [])
  hClose outRoute


  let handlerDir = "src" </> "Handler" </> module_
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

  mapM_ (generateHandler mode outHandler module_) tables
  hClose outHandler


generateModel :: FilterMode -> Handle -> Table -> IO ()
generateModel mode out Table {..} = do
  hPrintf out "%s sql=%s\n" (model mode $ tableName) sqlName
  -- generate primary keys if composite
  let composites = filter ((==True) . columnIsPrimary) tableColumns
      -- fieldName' = show
      fieldName Column{..} = if (columnIsPrimary) && length composites == 1
                             then "Id"
                             else if columnName == "id"
                                  then "id_"
                                  else sanitize $ camelCase columnName
      go :: Column -> IO ()
      go col@(Column {..}) = do
        hPrintf out
          "    %s %s %s sql=%s%s\n"
          (fieldName col)
          (htype columnType)
          (if columnNullable then "Maybe" :: String else "")
          columnName
          (if columnNullable && (htype columnType == "Text") then " default=NULL" :: String else "") -- persistent bug workaround
  mapM go tableColumns
  when (length composites > 1)
       (  hPutStrLn out
       $  "    Primary "
       ++ unwords (map fieldName composites)
       )

  hPutStrLn out ""


generateRoute :: Handle -> String -> Table -> String -> IO ()
generateRoute out module_ Table {..} group = do
  hPrintf out "  /%s %s GET !title=%s %s\n" tableName (handler module_ tableName)  tableName group

handler :: String -> String -> String
handler module_ s = printf "%s%sR" module_ (capitalize $ camelCase s)

model :: FilterMode -> String -> String
model mode = capitalize . camelCase . singularize' .sanitize where
  singularize' = case mode of
    DC -> (++"T")
    _ -> singularize

generateHandler :: FilterMode -> Handle -> String -> Table -> IO ()
generateHandler mode out module_ Table {..} = do
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
    (model mode tableName)








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
