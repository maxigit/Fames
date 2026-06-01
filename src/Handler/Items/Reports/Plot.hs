module Handler.Items.Reports.Plot
( plotWidget
, toXY
, opacity
, traceName
, bar 
)
where
import Import hiding(unzip)
import Data.Functor(unzip)
import Data.Aeson.QQ(aesonQQ)


plotWidget :: [ Value ] -> Maybe Int -> [ [  Value ] ] -> Widget
plotWidget layouts heightm tracess = do
   plotId <- newIdent
   let height = fromMaybe 400 heightm
       traces = fmap mconcat tracess
       plot = toWidgetBody [julius|
            Plotly.newPlot( #{toJSON plotId}
                      , #{toJSON traces}
                      , #{mconcat layouts}
                      );
            |] :: Widget
   [whamlet|
     <div id=#{plotId} style="height:#{height}">
       ^{plot}
   |]

       
       
toXY :: (Functor f, ToJSON (f a), ToJSON (f b)) => f (a, b) -> Value
toXY xys =
    let (xs, ys) = unzip xys 
    in [aesonQQ|{ x: #{toJSON xs}
                , y: #{toJSON ys}
                }
               |]

traceName :: Text -> Value
traceName name = [aesonQQ| { name: #{name} } |]

opacity :: Double -> Value
opacity op = [aesonQQ| { opacity: #{op} }|]


bar :: Value
bar = [aesonQQ| { type: "bar"
                }
      |]
