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
  withArgs yml $ handlerWith (\s -> s { appStaticDir = ".", appSkipStyleCategory = True, appRunMigration = False}) do
      run <- askRunInIO 
      let dispatch path nestedLevel section = run $ importFamesDispatch' path nestedLevel section
      liftIO $ withArgs others $ W.defaultMainWith dispatch
