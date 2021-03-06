{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Auth.NoPassword
import qualified Yesod.Auth.Message as Msg
import           Control.Applicative      ((<$>))
import qualified Db.Users as Users
import qualified Utils.Email as Email

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
    }

data HyperlinkMenuItem = HyperlinkMenuItem
  { hyperlinkMenuItemLabel          :: Text
  , hyperlinkMenuItemRoute          :: Route App
  , hyperlinkMenuItemAccessCallback :: Bool
  , hyperlinkMenuItemRelatedRoutes  :: [Route App]
  }

data DropdownMenuItem = DropdownMenuItem
  { dropdownMenuItemLabel           :: Text
  , dropdownMenuItemAccessCallback  :: Bool
  , dropdownMenuItemChildren        :: [HyperlinkMenuItem]
  }

data MenuItem = Hyperlink HyperlinkMenuItem | Dropdown DropdownMenuItem

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

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
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

newtype UserLoginForm = UserLoginForm { _loginEmail :: Text }

isRouteMatch :: Maybe (Route App) -> Route App -> [Route App] -> Bool
isRouteMatch (Just (EditPollR _)) PollsR _ = True
isRouteMatch (Just (ViewPollR _)) PollsR _ = True
isRouteMatch (Just CreatePollR  ) PollsR _ = True
isRouteMatch (Just currentRoute) route relatedRoutes =
  currentRoute == route || elem currentRoute relatedRoutes
isRouteMatch Nothing route _ = route == HomeR

runDatabaseAction
  :: (HandlerSite m ~ App, MonadUnliftIO m, MonadHandler m)
  => ReaderT SqlBackend m b
  -> m b
runDatabaseAction action = do
  master <- getYesod
  runSqlPool action $ appConnPool master

isLoggedIn :: HandlerFor App AuthResult
isLoggedIn = maybeAuthId
  >>= maybe (return AuthenticationRequired) (const $ return Authorized)

handleErrors :: Widget -> String -> Handler TypedContent
handleErrors content errorMessage = do
  layout <- maybe authLayout (const defaultLayout) <$> maybeAuthId
  selectRep $ do
    provideRep $ layout content
    provideRep $ return $ object ["message" .= errorMessage]

genericErrors :: String -> String -> Handler TypedContent
genericErrors errorNumber errorMessage =
  handleErrors $(widgetFile "errors/generic") errorMessage

menuItemAccessCallback :: MenuItem -> Bool
menuItemAccessCallback (Hyperlink h) = hyperlinkMenuItemAccessCallback h
menuItemAccessCallback (Dropdown d) = dropdownMenuItemAccessCallback d

addScripts :: Widget
addScripts = mapM_ addScriptRemote [
  "https://cdn.jsdelivr.net/npm/date-input-polyfill@2.14.0/date-input-polyfill.dist.min.js"
 ,"https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.22.2/moment.min.js"
 ,"https://cdnjs.cloudflare.com/ajax/libs/ramda/0.25.0/ramda.min.js"
 ,"https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.min.js"
 ]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just UserLoginR

    isAuthorized :: Route App -> Bool -> Handler AuthResult
    isAuthorized HomeR _ = isLoggedIn
    isAuthorized CreatePollR _ = isLoggedIn
    isAuthorized PollsR _ = isLoggedIn
    isAuthorized (EditPollR friendlyUrl) _ = do
      mUser <- maybeAuthId
      case mUser of
        Nothing -> return AuthenticationRequired
        Just userId -> do
          polls <- runDB $ selectList [ PollFriendlyUrl ==. friendlyUrl, PollCreatedByUserId ==. userId ] [LimitTo 1]
          case polls of
            [_] -> return Authorized
            _   -> return $ Unauthorized "You are not the creator of this poll"
    isAuthorized (ViewPollR _) _ = isLoggedIn
    isAuthorized GameNightsR _ = isLoggedIn
    isAuthorized UserPreferencesR _ = isLoggedIn
    isAuthorized (DeletePollR _) _ = isLoggedIn
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized UserLoginR _ = return Authorized
    isAuthorized EmailSentR _ = return Authorized

    errorHandler :: ErrorResponse -> Handler TypedContent
    errorHandler NotFound = genericErrors "404" "Page could not be found"
    errorHandler (PermissionDenied _) = genericErrors "403" "Permission denied"
    errorHandler _ = genericErrors ":(" "Oops, something went wrong"

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mcurrentRoute <- getCurrentRoute
        mUserId <- maybeAuthId
        case mUserId of
            Nothing -> notFound
            Just userId -> do
              mUser <- runDB $ getEntity userId
              case mUser of
                Nothing -> notFound
                Just (Entity _ user) -> do
                  -- Define the menu items of the header.
                  let menuItems =
                          [ NavbarLeft $ Hyperlink $ HyperlinkMenuItem
                              { hyperlinkMenuItemLabel          = "Home"
                              , hyperlinkMenuItemRoute          = HomeR
                              , hyperlinkMenuItemAccessCallback = True
                              , hyperlinkMenuItemRelatedRoutes  = []
                              }
                          , NavbarLeft $ Hyperlink $ HyperlinkMenuItem
                              { hyperlinkMenuItemLabel          = "Polls"
                              , hyperlinkMenuItemRoute          = PollsR
                              , hyperlinkMenuItemAccessCallback = True
                              , hyperlinkMenuItemRelatedRoutes  = [CreatePollR]
                              }
                          , NavbarLeft $ Hyperlink $ HyperlinkMenuItem
                              { hyperlinkMenuItemLabel          = "Game Nights"
                              , hyperlinkMenuItemRoute          = GameNightsR
                              , hyperlinkMenuItemAccessCallback = True
                              , hyperlinkMenuItemRelatedRoutes  = []
                              }
                          , NavbarRight $ Dropdown $ DropdownMenuItem
                              { dropdownMenuItemLabel           = userEmail user
                              , dropdownMenuItemAccessCallback  = True
                              , dropdownMenuItemChildren        = [
                                HyperlinkMenuItem
                                  { hyperlinkMenuItemLabel          = "User Preferences"
                                  , hyperlinkMenuItemRoute          = UserPreferencesR
                                  , hyperlinkMenuItemAccessCallback = True
                                  , hyperlinkMenuItemRelatedRoutes  = []
                                  }
                              , HyperlinkMenuItem
                                  { hyperlinkMenuItemLabel          = "Log Out"
                                  , hyperlinkMenuItemRoute          = AuthR LogoutR
                                  , hyperlinkMenuItemAccessCallback = True
                                  , hyperlinkMenuItemRelatedRoutes  = []
                                  }
                                ]
                              }
                          ]

                  let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
                  let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

                  let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
                  let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
                  -- We break up the default layout into two components:
                  -- default-layout is the contents of the body tag, and
                  -- default-layout-wrapper is the entire page. Since the final
                  -- value passed to hamletToRepHtml cannot be a widget, this allows
                  -- you to use normal widget features in default-layout.
                  pc <- widgetToPageContent $ do
                    addScripts
                    addStylesheet $ StaticR $ StaticRoute ["css", "styles.css"] []

                    $(widgetFile "default-layout")
                  withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
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
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB = runDatabaseAction

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

instance YesodAuthPersist App where
    type AuthEntity App = User

instance YesodAuth App where
    type AuthId App = UserId

    authenticate Creds{..} = do
        mUser <- liftHandler $ runDatabaseAction $ Users.getUserByEmail credsIdent
        case mUser of
            Nothing -> return $ UserError Msg.Email
            Just (Entity userId _) -> return $ Authenticated userId
    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [authNoPassword]

    authLayout widget = liftHandler $ do
        master <- getYesod

        pc <- widgetToPageContent $ do
          addScripts
          addStylesheet $ StaticR $ StaticRoute ["css", "styles.css"] []
          $(widgetFile "auth-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance NoPasswordAuth App where
    -- | Route to a page that dispays a login form. This is not provided by
    -- the plugin.
    loginRoute :: App -> Route App
    loginRoute _ = UserLoginR

    -- | Route to which the user should be sent after entering an email
    -- address. This is not provided by the plugin.
    --
    -- __Note__: the user will not be authenticated when they reach the page.
    emailSentRoute :: App -> Route App
    emailSentRoute _ = EmailSentR

    -- | Send a login email.
    sendLoginEmail :: Email -- ^ The email to send to
                   -> Text  -- ^ The URL that will log the user in
                   -> AuthHandler App ()
    sendLoginEmail email url = do
        mailSettings <- appMail . appSettings <$> getYesod
        results <- liftIO . Email.sendLoginEmail email url $ mailSettings
        case results of
            Left _ -> redirect UserLoginR
            Right () -> return ()

    -- | Get a user by their email address. Used to determine if the user exists or not.
    getUserByEmail :: Email -> AuthHandler App (Maybe (AuthId App))
    getUserByEmail email =
        (fmap . fmap $ entityKey) <$> runDatabaseAction $ Users.getUserByEmail email

    -- | Get a Hash by a TokenId.
    --
    -- Invoked when the user returns to the site from an email. We don't know
    -- who the user is at this point as they may open the link from the email
    -- on another device or in another browser, so session data can't be used.
    -- Equally we do not want to pass the user's ID or email address in a URL
    -- if we don't have to, so instead we look up users by the 'TokenId' that
    -- we issued them earlier in the process.
    getEmailAndHashByTokenId :: TokenId -> AuthHandler App (Maybe (Email, Hash, AuthId App))
    getEmailAndHashByTokenId token = do
        mUser <- runDatabaseAction $ Users.getUserByTokenId token
        return $ do
            (Entity userId User{..}) <- mUser
            hash' <- userHash
            return (userEmail, hash', userId)

    -- | Update a user's login hash
    --
    -- This is also used to blank out the hash once the user has logged in, or
    -- can be used to prevent the user from logging in, so must accept a value
    -- of `Nothing`.
    --
    -- /It is recommended that the/ 'TokenId' /storage be enforced as unique/.
    -- For this reason, the token is not passed as a maybe, as some storage
    -- backends treat `NULL` values as the same.
    updateLoginHashForUser :: (AuthId App) -> Maybe Hash -> TokenId -> AuthHandler App ()
    updateLoginHashForUser authId hash' token =
        runDatabaseAction $
            Users.getUser authId >>=
                maybe (return ())
                      (\user -> Users.updateUser authId (user { userHash = hash'
                                                              , userToken = token
                                                              }))


    -- | Create a new user with an email address and hash.
    newUserWithLoginHash :: Email -> Hash -> TokenId -> AuthHandler App ()
    newUserWithLoginHash email hash' token = do
        _ <- runDatabaseAction $ Users.createUser (User { userEmail = email
                                                        , userNickname = Nothing
                                                        , userHash = Just hash'
                                                        , userToken = token
                                                        , userBggUsername = Nothing
                                                        })
        return ()

    -- | Get the settings for the NoPassword Plugin.
    settings :: AuthHandler App NoPasswordSettings
    settings =
        NoPasswordSettings <$> (appEmailTimeout . appSettings <$> getYesod)

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
