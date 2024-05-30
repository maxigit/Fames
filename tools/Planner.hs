import ClassyPrelude hiding (getArgs)
import qualified WarehousePlanner.Main as W
import Handler.Planner.FamesImport
import Application (handlerWith)
import System.Environment(getArgs, withArgs)
import Settings(AppSettings(..))


main:: IO ()
main = do
  args <- getArgs
  let (yml, others) = partition (".yml" `isSuffixOf`) args
  withArgs yml $ handlerWith (\s -> s { appStaticDir = "."}) do
      run <- askRunInIO 
      let dispatch path section = run $ importFamesDispatch' path section
      liftIO $ withArgs others $ W.defaultMainWith dispatch
