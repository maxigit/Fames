module Handler.Items.Reports.Sources
where 

import Import



-- itemSalesParam 
itemSalesSource :: SqlConduit () 
itemsalesSource = let
  sqlSelect = "SELECT ??, 0_debtor_trans.tran_date, 0_debtor_trans.debtor_no, 0_debtor_trans.branch_code, 0_debtor_trans.order_, loc_code"
  sql0 = intercalate " " $
          " FROM 0_debtor_trans_details " :
          "JOIN 0_debtor_trans ON (0_debtor_trans_details.debtor_trans_no = 0_debtor_trans.trans_no " :
          "                    AND 0_debtor_trans_details.debtor_trans_type = 0_debtor_trans.type)  " :
          (if rpShowInactive param then "" else "JOIN 0_stock_master USING (stock_id)") :
          " JOIN 0_stock_moves using(trans_no, type, stock_id, tran_date)" : -- ignore credit note without move => damaged
          (fromMaybe "" stockJoin ) :
          "WHERE type IN ("  :
          (tshow $ fromEnum ST_CUSTDELIVERY) :
          ",":
          (tshow $ fromEnum ST_CUSTCREDIT) :
          ") " :
          "AND qty_done != 0" :
          ("AND stock_id LIKE '" <> stockLike <> "'") : -- we don't want space between ' and stockLike
          -- " LIMIT 100" :
          []
  (w,concat -> p) = unzip $ (if rpShowInactive param then Nothing else Just (" AND inactive = False ", []))
                            ?: (fmap (\w -> (" AND " <> w, stockParams)) stockWhere )
                            ?: toT'Ps (generateTranDateIntervals param)

  sql = sql0 <> intercalate " "  w
  in rawQuery (sqlSelect <> sql) p
   
