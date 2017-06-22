{-# LANGUAGE OverloadedStrings #-}
module Handler.WHStockAdjustmentSpec where

import TestImport
import Handler.WH.StockAdjustment
import SharedStockAdjustment
import Yesod.Auth (requireAuthId)
import FA hiding(User)

spec :: Spec
spec = parallel pureSpec >> appSpec

lostLoc = FA.LocationKey "LOST" :: FA.LocationId
defLoc = FA.LocationKey "DEF" :: FA.LocationId

prepareDB = do
      Just (Entity userId user) <- runDB $  selectFirst [] []
      let types = user :: User
      
      processedAt <- liftIO getCurrentTime
      runDB $ do
        Just jack <- insertUnique $ Operator "John" "Smith" "Jack" True
        Just doc <- insertUnique $ DocumentKey "key" "test" "ref0" "" userId  processedAt
        insertMany_ [ (Stocktake sku qty sku  1 defLoc
                                 (fromGregorian 2017 05 26)
                            True jack Nothing doc [] Nothing) :: Stocktake
                    | (sku, qty) <- [("A", 5), ("B", 7), ("C", 14)
                                    , ("Before", 4), ("After", 4), ("Before'" , 4), ("After'", 4)
                                    ]
                    ]

lengthShouldBe' filter all expected = do
  adjs <- runDB $ selectList [] []
  -- traceShowM adjs
  liftIO $ length (adjs :: [Entity StockAdjustmentDetail]) `shouldBe` all
  l <- runDB $ count filter
  liftIO $ l `shouldBe` expected

lengthShouldBe filter expected = lengthShouldBe' filter expected expected

printAdjustments = do
  adjs <- runDB $ selectList [] []
  mapM_ (print. entityVal) (adjs :: [Entity StockAdjustmentDetail])


postAdjustment modulo sku def lost = do
  logAsAdmin
  prepareDB
  runDB $ do
    deleteWhere [StockMoveStockId ==. sku]
    when (def >( 0 :: Double)) $ do
      insert_ $ StockMove 1 sku 1 "DEF" (fromGregorian 2016 05 25) Nothing 0 "" def 0 0 True
    when (lost >( 0 :: Double)) $ do
      insert_ $ StockMove 1 sku 1 "LOST" (fromGregorian 2016 05 25) Nothing 0 "" lost 0 0 True
    insert_ $ StockMove 1 "Before" 1 "DEF" (fromGregorian 2017 05 25) Nothing 0 "" (-1) 0 0 True
    insert_ $ StockMove 1 "After" 1 "DEF" (fromGregorian 2017 05 25) Nothing 0 "" (-1) 0 0 True
    insert_ $ StockMove 1 "Before'" 1 "DEF" (fromGregorian 2017 05 27) Nothing 0 "" (-1) 0 0 True
    insert_ $ StockMove 1 "After'" 1 "DEF" (fromGregorian 2017 05 27) Nothing 0 "" (-1) 0 0 True
  get (WarehouseR WHStockAdjustmentR)
  statusIs 200
  request $ do
    setMethod "POST"
    setUrl (WarehouseR WHStockAdjustmentR)
    addToken_ "form#stock-adjustement"
    addPostParam "f2" "no" -- download
    byLabel "sort by" (tshow $ 1)
    byLabel "mode" (tshow $ 3)
    forM modulo $ \m -> byLabel "modulo" (tshow (m :: Int))
    addPostParam "action" "save"
    mapM_ (\sku' -> addPostParam ("active-"<>sku') (if sku' == sku then "on" else "" ))
          ["A", "B", "C", "Before", "After", "Before'", "After'"]
    addPostParam "Before" "-1"
    addPostParam "After" "0"
    addPostParam "Before'" "-1"
    addPostParam "After'" "0"
  statusIs 200
   
  
  
appSpec = withAppWipe BypassAuth $ describe "StockAdjustment" $ do
  context "without modulo" $ do
    it "create missing adjustment" $ do
      postAdjustment (Nothing :: Maybe Int) ("A" ) 6 (0)
      [ StockAdjustmentDetailStockId ==. "A"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. (Just defLoc)
        , StockAdjustmentDetailTo ==. (Just lostLoc)
        ] `lengthShouldBe` 1
    it "create new adjustment" $ do
      postAdjustment Nothing "B" 6 0
      [ StockAdjustmentDetailStockId ==. "B"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. Nothing
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe` 1
    it "create found adjustment" $ do
      postAdjustment Nothing "B" 6 1
      [ StockAdjustmentDetailStockId ==. "B"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. (Just lostLoc)
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe` 1
    it "create lost new adjustment" $ do
      postAdjustment Nothing "B" 4 1
      [ StockAdjustmentDetailStockId ==. "B"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. (Just lostLoc)
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe'` 2 $ 1
      [ StockAdjustmentDetailStockId ==. "B"
        , StockAdjustmentDetailQuantity ==. 2
        , StockAdjustmentDetailFrom ==. Nothing
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe'`  2 $ 1
  context "before/after " $ do
    context "with move before" $ do
      it "remove before moves from qoh" $ do
        postAdjustment Nothing "Before" 5 0
        [ StockAdjustmentDetailStockId ==. "Before"
          -- , StockAdjustmentDetailQuantity ==. 1
          -- , StockAdjustmentDetailFrom ==. (Just defLoc)
          -- , StockAdjustmentDetailTo ==. (Just lostLoc)
          ] `lengthShouldBe` 0
      it "doesn't remove after moves from qoh" $ do
        postAdjustment Nothing "After" 5 0
        [ StockAdjustmentDetailStockId ==. "After"
          , StockAdjustmentDetailQuantity ==. 1
          , StockAdjustmentDetailFrom ==. (Just defLoc)
          , StockAdjustmentDetailTo ==. (Just lostLoc)
          ] `lengthShouldBe` 1
    context "with move after" $ do
      it "remove before moves from qoh" $ do
        postAdjustment Nothing "Before'" 5 0
        [ StockAdjustmentDetailStockId ==. "Before'"
          -- , StockAdjustmentDetailQuantity ==. 1
          -- , StockAdjustmentDetailFrom ==. (Just defLoc)
          -- , StockAdjustmentDetailTo ==. (Just lostLoc)
          ] `lengthShouldBe` 0
      it "doesn't remove after moves from qoh" $ do
        postAdjustment Nothing "After'" 5 0
        [ StockAdjustmentDetailStockId ==. "After'"
          , StockAdjustmentDetailQuantity ==. 1
          , StockAdjustmentDetailFrom ==. (Just defLoc)
          , StockAdjustmentDetailTo ==. (Just lostLoc)
          ] `lengthShouldBe` 1
  context "with modulo" $ do
    it "create missing adjustment" $ do
      postAdjustment (Just 6) "A" 0 0
      [ StockAdjustmentDetailStockId ==. "A"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. (Just defLoc)
        , StockAdjustmentDetailTo ==. (Just lostLoc)
        ] `lengthShouldBe` 1
    it "create new adjustment" $ do
      postAdjustment (Just 6) "B" 0 0
      [ StockAdjustmentDetailStockId ==. "B"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. Nothing
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe` 1
    it "create found adjustment" $ do
      postAdjustment (Just 6) "B" 0 1 
      [ StockAdjustmentDetailStockId ==. "B"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. (Just lostLoc)
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe` 1
    it "create lost new adjustment" $ do
      postAdjustment (Just 6) "C" 0 1
      [ StockAdjustmentDetailStockId ==. "C"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. (Just lostLoc)
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe'` 2 $ 1
      [ StockAdjustmentDetailStockId ==. "C"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. Nothing
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe'` 2 $ 1
  context "many moves (bugs)" $ do
    -- In this case the stock take has been correct
    -- but multiple pickings induceds a fake stocktake
    -- the initial QOH is 8
    -- someone picked 1 leaving 7
    -- the second picker notice there is only 7 (which is correct) instead of 8
    -- We therefore get a stocktake for 7.
    -- In this case the first move (pick) has been done before the stocktake of 7
    -- whereas the second moves has been done after.
    -- A post with -1 before should not generate any moves
    let postA qBefores = do
            logAsAdmin
            Just (Entity userId user) <- runDB $  selectFirst [] []
            let types = user :: User
            
            processedAt <- liftIO getCurrentTime
            runDB $ do
              Just jack <- insertUnique $ Operator "John" "Smith" "Jack" True
              Just doc <- insertUnique $ DocumentKey "key" "test" "ref0" "" userId  processedAt
              insertMany_ [ (Stocktake sku qty sku  1 defLoc
                                       (fromGregorian 2017 06 21)
                                  True jack Nothing doc [] Nothing) :: Stocktake
                          | (sku, qty) <- [("Bug", 7)]
                          ]
            runDB $ do
              deleteWhere [StockMoveStockId ==. "Bug"]
              insert_ $ StockMove 1 "Bug" 1 "DEF" (fromGregorian 2016 05 25) Nothing 0 "" 8 0 0 True
              insert_ $ StockMove 1 "Bug" 1 "DEF" (fromGregorian 2017 06 21) Nothing 0 "" (-1) 0 0 True
              insert_ $ StockMove 1 "Bug" 1 "DEF" (fromGregorian 2017 06 21) Nothing 0 "" (-1) 0 0 True
            get (WarehouseR WHStockAdjustmentR)
            statusIs 200
            request $ do
              setMethod "POST"
              setUrl (WarehouseR WHStockAdjustmentR)
              addToken_ "form#stock-adjustement"
              addPostParam "f2" "no" -- download
              byLabel "sort by" (tshow $ 1)
              byLabel "mode" (tshow $ 3)
              byLabel "modulo" "6"
              addPostParam "action" "save"
              addPostParam "active-Bug" "on"
              mapM_ (addPostParam "Bug") (map tshow qBefores)
            statusIs 200
    it "new one" $ do
      postA [-1, -1]
      printAdjustments
      [ StockAdjustmentDetailStockId ==. "Bug"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. Nothing
        , StockAdjustmentDetailTo ==. (Just defLoc)
        ] `lengthShouldBe` 1
    it "nothing happend" $ do
      postA [-1,0]
      [ StockAdjustmentDetailStockId ==. "Bug"
        ] `lengthShouldBe` 0
      printAdjustments
    it "lose one" $ do
      postA [0,0]
      printAdjustments
      [ StockAdjustmentDetailStockId ==. "Bug"
        , StockAdjustmentDetailQuantity ==. 1
        , StockAdjustmentDetailFrom ==. (Just defLoc)
        , StockAdjustmentDetailTo ==. (Just lostLoc)
        ] `lengthShouldBe` 1
    
      
          

pureSpec = describe "@pure @parallel" $ do
  workingDaysSpec
  computeBadgesSpec

workingDaysSpec = describe "WorkingDays" $ do
  context "next working day without rolling" $ do
     it"get next day" $ do
       nextWorkingDay (fromGregorian 2017 05 22) `shouldBe` (fromGregorian 2017 05 23)
     it "get previous day" $ do
       previousWorkingDay (fromGregorian 2017 05 23) `shouldBe` (fromGregorian 2017 05 22)
  context "next working day rolling" $ do
     it "get next monday day" $ do
       nextWorkingDay (fromGregorian 2017 05 19) `shouldBe` (fromGregorian 2017 05 22)
     it "get previous friday day" $ do
       previousWorkingDay (fromGregorian 2017 05 22) `shouldBe` (fromGregorian 2017 05 19)

computeBadgesSpec = describe "computeBadges" $ do
    let o0 = OriginalQuantities 0 0 0 Nothing
        b0 = BadgeQuantities 0 0 0 0 0
    it "find 1 missing" $ do
      computeBadges (OriginalQuantities 5 6 1 Nothing)
        `shouldBe` b0 {bMissing = 1}
    it "find 1 new" $ do
      computeBadges (OriginalQuantities 7 6 0 Nothing)
        `shouldBe` b0 {bNew = 1}
    it "find 1 found" $ do
      computeBadges (OriginalQuantities 7 6 5 Nothing)
        `shouldBe` b0 {bFound = 1}
    it "bug " $ do
      computeBadges (OriginalQuantities 12 11 1 Nothing)
        `shouldBe` b0 {bFound = 1}
      computeBadges (OriginalQuantities 6 9 0 (Just 6))
        `shouldBe` b0 {bMissing = 3}
    it "find some found and some new" $ do
      computeBadges o0 {qtake = 12, qoh = 5, qlost = 4}
        `shouldBe` b0 {bFound = 4, bNew = 3}
    describe "using modulo" $ do 
      let o6 = o0 {qModulo = Just 6}
      context "missing in stocktake" $ do
        it "find 1 missing" $ do
          computeBadges o6 {qtake = 5, qoh = 12}
            `shouldBe` b0 {bMissing = 1, bMissingMod = 6}
        it "find 1 too many:new" $ do
          computeBadges o6 {qtake = 7, qoh = 12}
            `shouldBe` b0 {bNew =1, bMissingMod = 6 }
        it "find 1 too many:lost" $ do
          computeBadges o6 {qtake = 7, qoh = 12, qlost = 1}
            `shouldBe` b0 {bFound =1, bMissingMod = 6 }
      context "missing in FA" $ do 
        context "new" $ do
          it "find 1 missing" $ do
            computeBadges o6 {qtake = 5}
              `shouldBe` b0 {bMissing = 1, bFoundMod = 6}
          it "find 1 too many (new)" $ do
            computeBadges o6 {qtake = 7}
              `shouldBe` b0 {bNew = 1, bFoundMod = 6}
          it "find 1 too many (found)" $ do
            computeBadges o6 {qtake = 7, qlost = 1}
              `shouldBe` b0 {bFound = 1, bFoundMod = 6}
          it "find 0 missing" $ do
            computeBadges o6 {qtake = 5, qoh=5}
              `shouldBe` b0 
        context "lost" $ do
          it "find all missing" $ do
            computeBadges o6 {qtake = 5, qlost=5}
              `shouldBe` b0 {bFound=5}
          it "find 1 too many" $ do
            computeBadges o6 {qtake = 8, qlost = 1}
              `shouldBe` b0 {bNew = 1, bFound = 1, bFoundMod = 6}
      context "edge case" $ do
        -- 3 missing should be seen as missing, not as found
            it "find 2 too many" $ do
              computeBadges o6 {qtake = 2}
                `shouldBe` b0 {bNew = 2}
            it "find 3 missing" $ do
              computeBadges o6 {qtake = 3}
                `shouldBe` b0 {bMissing = 3, bFoundMod = 6}
            it "find 2 missing" $ do
              computeBadges o6 {qtake = 4}
                `shouldBe` b0 {bMissing = 2, bFoundMod = 6}
      context "remove full boxes missing" $ do
        it "cancels 12 missing" $ do
              computeBadges o6 {qtake = 6, qoh = 24}
                `shouldBe` b0 {bMissingMod = 18}
        it "cancels 12 found" $ do
              computeBadges o6 {qtake = 24, qoh = 6}
                `shouldBe` b0 {bFoundMod = 18}
      context "modulo not needed" $ do
        it "find 1 missing" $ do
          computeBadges o6 {qtake = 5, qoh = 6, qlost = 1}
            `shouldBe` b0 {bMissing = 1}
        it "find 1 new" $ do
          computeBadges o6 {qtake = 7, qoh = 6}
            `shouldBe` b0 {bNew = 1}
        it "find 1 found" $ do
          computeBadges o6 {qtake = 7, qoh = 6, qlost = 5}
            `shouldBe` b0 {bFound = 1}
        it "bug " $ do
          computeBadges o6 {qtake = 12, qoh = 11, qlost = 1}
            `shouldBe` b0 {bFound = 1}
        it "find some found and some new" $ do
          computeBadges o6 {qtake = 12, qoh = 4, qlost = 1}
            `shouldBe` b0 {bFound = 1, bNew = 1, bFoundMod =6}
      context "bugs" $ do
        it "find not too much some found and some new" $ do
          computeBadges o6 {qtake = 8, qoh = 20, qlost = 9}
            `shouldBe` b0 {bMissingMod = 12}
