import ClassyPrelude hiding (getArgs)
import qualified WarehousePlanner.Main as W
import Handler.Planner.FamesImport
import Application (handler)
import System.Environment(getArgs, withArgs)


main:: IO ()
main = do
  args <- getArgs
  let (yml, others) = partition (".yml" `isSuffixOf`) args
  let dispatch path section = withArgs yml $ handler (importFamesDispatch' path section)
  withArgs others $ W.defaultMainWith dispatch
