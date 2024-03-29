{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module FA (module X) where 
import FAxx59 as X
import FAxx43 as X
import FAxx49 as X
import FAxx10 as X
import FAxx91 as X
import FAxx13 as X
import FAxx71 as X
import FAxx48 as X
import FAxx06 as X
import FAxx75 as X
import FAxx64 as X
import FAxx16 as X
import FAxx94 as X
import FAxx56 as X
import FAxx20 as X
import FAxx12 as X
import FAxx04 as X
import FAxx15 as X
import FAxx32 as X
import FAxx55 as X
import FAxx14 as X
import FAxx89 as X
import FAxx88 as X
import FAxx57 as X
import FAxx36 as X
import FAxx85 as X
import FAxx84 as X
import FAxx76 as X
import FAxx09 as X
import FAxx08 as X
import FAxx07 as X
import FAxx05 as X
import FAxx87 as X
import FAxx03 as X
import FAxx19 as X
import FAxx18 as X
import FAxx17 as X
import FAxx74 as X
import FAxx30 as X
import FAxx35 as X
import FAxx81 as X
import FAxx60 as X
import FAxx78 as X
import FAxx61 as X
import FAxx83 as X
import FAxx51 as X
import FAxx41 as X
import FAxx28 as X
import FAxx27 as X
import FAxx26 as X
import FAxx67 as X
-- import FAxx97 as X
import FAxx92 as X
import FAxx82 as X
import FAxx45 as X
import FAxx72 as X
import FAxx33 as X
import FAxx62 as X
import FAxx01 as X
import FAxx68 as X
import FAxx52 as X
import FAxx54 as X
import FAxx11 as X
import FAxx58 as X
import FAxx42 as X
import FAxx50 as X
import FAxx93 as X
import FAxx00 as X
import FAxx39 as X
import FAxx38 as X
import FAxx37 as X
import FAxx96 as X
import FAxx21 as X
import FAxx44 as X
import FAxx77 as X
import FAxx46 as X
import FAxx25 as X
import FAxx29 as X
import FAxx73 as X
import FAxx02 as X
import FAxx90 as X
import FAxx80 as X
import FAxx70 as X
import FAxx47 as X
import FAxx66 as X
import FAxx40 as X
import FAxx63 as X
import FAxx65 as X
import FAxx24 as X
import FAxx34 as X
import FAxx86 as X
import FAxx53 as X
import FAxx22 as X
import FAxx23 as X
import FAxx79 as X
import FAxx31 as X
import FAxx69 as X
import FAxx95 as X

import ClassyPrelude.Yesod
import qualified Language.Haskell.TH as TH

$(do
  dss <- mapM
    (\eName -> do
       let eType  = return $ TH.ConT eName
           keyname = TH.nameBase eName ++ "Key"
       Just keyCon <- TH.lookupValueName $ keyname
       Just keyUn <- TH.lookupValueName $ "un" ++ keyname
       [d| instance ToBackendKey SqlBackend $eType where
            toBackendKey =  toEnum . $(return $ TH.VarE keyUn)
            fromBackendKey =  $(return $ TH.ConE keyCon) . fromEnum
         |])
    [''Supplier
    ]
  return $ concat dss
  )
