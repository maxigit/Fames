{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Foundation
( module Foundation
, module RoutePiece
, module Role
, A(..)
, preCache
, cacheForEver
, cacheSecond
) where


import Import.NoFoundation hiding(toList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import qualified Yesod.Auth.Message    as Msg
import Yesod.Form (ireq, runInputPost, textField)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Fay
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified FA as FA
import qualified Crypto.Hash as Crypto
import Crypto.Hash (MD5, Digest)
import Role
import RoutePiece
import Util.SameCons-- (SameCons, sameCons)
import Util.EnumTree
import GHC.Generics(Generic)
import Data.Foldable(toList)
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as Text
import Util.Cache

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appFayCommandHandler :: CommandHandler App
    , appCache :: MVar ExpiryCache
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes.gen")

deriving instance Generic (Route App)
instance SameCons (Route App)

-- | Handler within an SQL transaction
-- or code already within a runDB.
type SqlHandler = ReaderT SqlBackend Handler 

instance EnumTreeable Text where enumTree = EnumTree []
instance EnumTreeable Int64 where enumTree = EnumTree []

instance EnumTreeable (Route Static) where enumTree = EnumTree []
instance EnumTreeable (Route Auth) where enumTree = EnumTree []
instance EnumTreeable (Route FaySite) where enumTree = EnumTree []
deriving instance Generic (WarehouseR)
instance EnumTreeable (WarehouseR) 
deriving instance Generic (ItemsR)
instance EnumTreeable (ItemsR) 
deriving instance Generic (AdministratorR)
instance EnumTreeable (AdministratorR) 
deriving instance Generic (FA'R)
instance EnumTreeable (FA'R) 
deriving instance Generic (FAX'R)
instance EnumTreeable (FAX'R) 
deriving instance Generic (FAMES'R)
instance EnumTreeable (FAMES'R) 
deriving instance Generic (DC'R)
instance EnumTreeable (DC'R) 

instance EnumTreeable (Route App)

-- | Generate a tree of all possible (and sensible) routes
-- Using Generic instead of TH means we have to derive each subsites routes manually.
-- However, it's not really a problem, as forgetting it generate a compile error, which is
-- much better than having to manage the list manually and forget to add new routes.
routeTree = enumTree  :: EnumTree (Route App)
data A = A Int | B Int deriving (Show, Generic)
instance SameCons (A)

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        userM <- entityVal <$$> maybeAuth
        master <- getYesod
        msgs <- getMessages
        currentRoute <- getCurrentRoute
        mainNavLinks <- mainLinks
        suggested0 <- getSuggestedLinks
        suggested <- mapM (\l@(_,r,_) -> do
                                 a <- isAuthorized r False
                                 return $ (l, a /= Authorized))
                             suggested0
        let sideLinks0 = sideLinks currentRoute
        sideLinks' <- mapM (mapM (mapM (\l@(_,r) -> do
                                 a <- isAuthorized r False
                                 return $ (l, a /= Authorized))))
                             sideLinks0


        

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    -- authRoute _ = Just $ LoginR' --  AuthR LoginR
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized (StaticR _) False = return Authorized
    -- Default to Authorized for now.
    -- isAuthorized AdministratorR _ = isAdministrator

      

    isAuthorized route r = do
       let wreq = if r then WriteRequest else ReadRequest
       settings <- appSettings <$> getYesod
       mu <- maybeAuth
       -- anonymous users are given the role <anonymous>, which might
       -- give them access to the route.
       -- However, if it's not the error message is different :
       -- They are not "unauthorized", they need to log on.
       let denied = case mu of
                        Nothing -> AuthenticationRequired
                        Just _  -> Unauthorized "Permissions required"
           role = roleFor (appRoleFor settings) (userIdent . entityVal <$> mu)
           routeUrl = mconcat [ "/" <> p | p <- fst (renderRoute route)]
       
       if authorizeFromPath role routeUrl wreq
          || authorizeFromAttributes role (routeAttrs route) wreq
          then return Authorized
          else return denied

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

instance YesodJquery App
instance YesodFay App where

    fayRoute = FaySiteR

    yesodFayCommand render command = do
        master <- getYesod
        appFayCommandHandler master render command

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins _ = [authFA] -- [authOpenId Claimed []]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

authFA :: AuthPlugin App
authFA = AuthPlugin "fa" dispatch loginWidget
  where
    -- dispatch _ _ = undefined
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _ = notFound
    loginR = PluginR "fa" ["login"]
    loginWidget toMaster = do
      request <- getRequest
      [whamlet|
 $newline never
        <form #login-form method="post" action="@{toMaster loginR}">
          $maybe t <- reqToken request
            <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
          <table>
            <tr>
              <th>_{Msg.UserName}
              <td>
                 <input type="text" name="username" required>
            <tr>
              <th>_{Msg.Password}
              <td>
                 <input type="password" name="password" required>
            <tr>
              <td colspan="2">
                 <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
        |]
postLoginR :: HandlerT Auth (HandlerT App IO) TypedContent
postLoginR = do
  (username, password) <- lift ( runInputPost
                                 ((,) <$> ireq textField "username"
                                      <*> ireq passwordField "password")
                               )
  isValid <- lift $ validatePassword username password
  if isValid
    then lift (setCredsRedirect (Creds "fa" username []))
    else do
       userExists <- lift $ doesUserNameExist username
       loginErrorMessageI LoginR ( if userExists
                                      then Msg.InvalidUsernamePass
                                      else Msg.IdentifierNotFound username
                                 )
  where validatePassword username password = runDB $ do
           c <- count [ FA.UserUserId ==. username
                      , FA.UserPassword ==. cryptFAPassword password ]
           return $ c == 1
        doesUserNameExist username = runDB $ do
          c <- count [ FA.UserUserId ==. username ]
          return $ c >= 1
cryptFAPassword :: Text -> Text
cryptFAPassword text = let digest = Crypto.hash (encodeUtf8 text)  :: Digest MD5
            in tshow digest


mainLinks :: Handler [(Text, Route App, Bool)]
mainLinks = do
  currentRoute <- getCurrentRoute
  let links= [ ("General Ledger", GLEnterReceiptSheetR)
             , ("Items", ItemsR (ItemsIndexR Nothing))
             , ("Warehouse", WarehouseR WHStockAdjustmentR)
             , ("Admin", AdministratorR AIndexR)
             , ("FA",  FA'R FADebtorsMasterR)
             , ("FAX",  FAX'R FAXItemRequestsR)
             , ("FAMES",  FAMES'R FAMESFamesBarcodeSeedR)
             , ("DC",  DC'R DCNodeR)
             ]
      authorised (_, r) = do
        auth <- isAuthorized r False
        return $ auth == Authorized
      active = maybe (const False) sameCons currentRoute
  allowed <- filterM authorised links
  return [ (title, route, active route)
         | (title, route) <- allowed
         ]

sideLinks :: Maybe (Route App)  -> [(Text, [(Text, Route App)])]

sideLinks (Just route) = let
  tree = routeTree
  routes = filter (sameCons route) (Data.Foldable.toList tree)
  titles0 = map routeTitle routes
  -- filter routes without a title
  route'titles = mapMaybe sequence  (zip routes titles0)
  -- group title is given in config/route using @title
  -- The group title is given only once for each  group
  groups0 = map (routeGroup.fst) route'titles
  groupNames = snd $ List.mapAccumL (\last_ title -> let new = title <|> last_
                                                in (new, new)
                                )
                               Nothing groups0
  groups = List.groupBy ((==) `on` snd) (zip route'titles groupNames)
  in [ ( fromJust name
       , map (\((r,title),_) -> (title, r)) group_)
     | (group_@((_,name):_)) <- groups
     , isJust name
     ]
sideLinks _ = []

routeValue :: Text -> Route App -> Maybe Text
routeValue prefix route = let
  attrs = mapMaybe (stripPrefix prefix) (toList $ routeAttrs route)
  in headMay attrs
-- | Find the group name of a route, used to group link on the side bar
routeGroup :: Route App -> Maybe Text
routeGroup route = let
  in decodeTitle <$> routeValue "group=" route

-- | Find the title of a route, used to display links
routeTitle :: Route App -> Maybe Text
routeTitle route = let
  in decodeTitle <$> routeValue "title=" route
  
-- | Decode title (replace _ by blank)
decodeTitle :: Text -> Text
decodeTitle = Text.replace "_" " " 
    

-- * Add suggested links to the suggested links sidebar
linksKey :: Text
linksKey = "links"

pushLinks :: Text -> (Route App) -> [(Text, Text)] -> Handler ()
pushLinks title route params = do
  oldLinks <- getSuggestedLinks
  
  let new = (title, route, params)
      links = take 5  $ new: filter (/= new) oldLinks
  setSession linksKey (tshow links)


getSuggestedLinks :: Handler [(Text, Route App, [(Text, Text)])]
getSuggestedLinks = do
  text <- lookupSession linksKey
  let links = text >>= readMay
  return $ fromMaybe [] links



-- * Caching
preCache0 :: (Show k, Typeable a) => Bool -> CacheDelay -> k -> Handler a -> Handler (Delayed Handler a)
preCache0 force delay key action = do
  cache <- getsYesod appCache
  preCache force cache key action delay
 
preCache1 :: (Show k, Typeable a) => Bool -> CacheDelay -> k -> (k -> Handler a) -> Handler (Delayed Handler a)
preCache1 force delay param action = preCache0 force delay param (action param)

cache0 :: (Show k, Typeable a) => Bool -> CacheDelay -> k -> Handler a -> Handler a
cache0 force delay key action = do
  cache <- getsYesod appCache
  expCache force cache key action delay

clearAppCache :: Handler ()
clearAppCache = do
  cache <- getsYesod appCache
  lift $ clearExpiryCache cache

