module Handler.GL.Payroll where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))



-- * Handlers
-- ** upload and show timesheets
getGLPayroll :: Handler Html
getGLPayroll = return "todo"
postGLPayroll :: Handler Html
postGLPayroll = return "todo"

-- ** Indiviual timesheet
getGLPayrollViewR :: Int64 -> Handler Html
getGLPayrollViewR key = return "todo"

getGLPayrollEditR :: Int64 -> Handler Html
getGLPayrollEditR key = return "todo"
postGLPayrollEditR :: Int64 -> Handler Html
postGLPayrollEditR key = return "todo"

postGLPayrollRejectR :: Int64 -> Handler Html
postGLPayrollRejectR key = return "todo"
