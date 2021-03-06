{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This auth plugin for Yesod enabled simple passwordless authentication.
--
-- The only detail required from a user is an email address, and accounts are
-- either updated or created, depending on whether the account exists or not.
-- To actually log in, users are sent an email containing a link that
-- authenticates them and logs them in.
--
-- This plugin provides:
--
-- * Token generation
-- * Orchestration of the login process and email sending
-- * Receiving of the login form data via HTTP POST.
-- * Authentication of users once they return to the site from an email
--
-- This plugin /does not/ provide:
--
-- * A login form
-- * Email rendering or sending
-- * An account model
-- * A viewable interface (i.e. via HTTP GET) for the login form
--
-- These are left for the user of the plugin to implement so that they can
-- retain control over form functionality, account models, email design and
-- email service provider.
--
-- Implementation checklist:
--
-- 1. Implement an instance of 'NoPasswordAuth' for your Yesod application.
-- 2. Implement a Yesod form that resolves to an 'EmailForm'.
-- 3. Add `authNoPassword` to your authentication plugins in your instance of
--    `YesodAuth`, passing the form you wish to use for authentication. This
--    typeclass provides a number of methods for customisation of behaviour,
--    but the minimal implementation is:
--
--     * 'loginRoute'
--     * 'emailSentRoute'
--     * 'sendLoginEmail'
--     * 'getUserByEmail'
--     * 'getEmailAndHashByTokenId'
--     * 'updateLoginHashForUser'
--     * 'newUserWithLoginHash'
module Auth.NoPassword (
    -- * Plugin
      authNoPassword
    -- * Form Type
    , EmailForm(..)
    -- * Typeclass
    , NoPasswordAuth(..)
    -- * Types
    , Email
    , Token
    , TokenId
    , Hash
    -- ** Utility
    , loginPostR
    , NoPasswordSettings(..)
) where

import Prelude

import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Text.Blaze as B

import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

import Network.HTTP.Types.URI (urlEncode, urlDecode)

import Yesod.Core
import Yesod.Form
import Yesod.Auth
import Crypto.PasswordStore
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString.Base64 as E
import qualified Data.ByteString.Char8 as E
import Data.Time.ISO8601
import Data.Maybe
-- Constants

pluginName :: Text
pluginName = "email"

-- | Route to which the site should POST the email form form.
loginPostR :: AuthRoute
loginPostR = PluginR pluginName ["login"]


type Email = Text
type Token = Text
type TokenId = Text
type Hash = Text
type CurrentTime = Text


-- | Data type required for the Yesod form.
newtype EmailForm = EmailForm
    { efEmail :: Email
    } deriving (Show)

newtype NoPasswordSettings = NoPasswordSettings
    { noPasswordEmailTimeout :: NominalDiffTime
    }

-- | Function to create the Yesod Auth plugin. Must be used by a type with an
-- instance for 'NoPasswordAuth', and must be given a form to use.
authNoPassword :: NoPasswordAuth m
               => AuthPlugin m
authNoPassword = AuthPlugin pluginName dispatch login
    where
        login _ = error "NoPasswordAuth does not provide a login widget"
        dispatch "POST" ["login"] = postEmailR
        dispatch "GET"  ["login"] = getLoginR
        dispatch _ _ = notFound

postEmailR :: NoPasswordAuth m
           => AuthHandler m TypedContent
postEmailR = do
    let form = EmailForm <$> areq emailField "email" Nothing
    ((result, _), _) <- liftHandler $ runFormPost $ renderTable form
    master <- getYesod
    case result of
        FormMissing -> do
            setMessage "Something went wrong, please try again"
            redirect (loginRoute master)
        FormFailure as -> do
            mapM_ (setMessage . B.text) as
            redirect (loginRoute master)
        FormSuccess e -> do
            let email = efEmail e
            strength <- tokenStrength
            (hash, token, currentTime) <- liftIO $ genToken strength
            muser <- getUserByEmail (efEmail e)
            tid <- liftIO genTokenId
            case muser of
                Just user ->
                    updateLoginHashForUser user (Just hash) tid
                Nothing ->
                    newUserWithLoginHash email hash tid
            url <- genUrl token tid currentTime
            sendLoginEmail email url
            redirect (emailSentRoute master)


getLoginR :: NoPasswordAuth m => AuthHandler m TypedContent
getLoginR = do
    paramName <- tokenParamName
    loginParam <- lookupGetParam paramName
    emailTimeout <- noPasswordEmailTimeout <$> settings
    case unpackTokenParam loginParam of
        Nothing -> permissionDenied "Missing login token"
        Just (tid, loginToken, currTime) -> do
            mEmailHash <- getEmailAndHashByTokenId tid
            case mEmailHash of
                Nothing -> permissionDenied "Invalid login token"
                Just (email, hash, authId) -> do
                    actualCurrTime <- liftIO getCurrentTime
                    if verifyToken hash loginToken (convertTime currTime) actualCurrTime emailTimeout
                        then do
                            updateLoginHashForUser authId Nothing tid
                            setCredsRedirect (Creds pluginName email [])
                        else permissionDenied "Invalid login token"


unpackTokenParam :: Maybe Text -> Maybe (TokenId, Token, CurrentTime)
unpackTokenParam param = do
    p <- param
    case splitOn ":" p of
        [tid,tkn,currTime] -> Just (tid, tkn, currTime)
        _ -> Nothing


genToken :: Int -> IO (Hash, Token, CurrentTime)
genToken strength = do
    currentTime <- decodeUtf8 . urlEncode True . E.encode . E.pack . formatISO8601Millis <$> getCurrentTime
    tokenSalt <- genSaltIO
    let token = exportSalt tokenSalt
    hash <- makePassword token strength
    return (decodeUtf8 hash, decodeUtf8 (urlEncode True token), currentTime)

convertTime :: CurrentTime -> UTCTime
convertTime str =
    let minDate = UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)
        convert = fromMaybe minDate . parseISO8601 . E.unpack
        decoded = either (const minDate) convert . E.decode . urlDecode False . encodeUtf8 $ str
    in decoded

verifyToken :: Hash -> Token -> UTCTime -> UTCTime -> NominalDiffTime -> Bool
verifyToken hash token currTime actCurrTime emailTimeout = verifyPassword t h && timeMatches
    where
        h = encodeUtf8 hash
        t = urlDecode False (encodeUtf8 token)
        timeMatches = (diffUTCTime actCurrTime currTime) <= emailTimeout

genTokenId :: IO TokenId
genTokenId = U.toText <$> U.nextRandom


genUrl :: NoPasswordAuth m => Token -> TokenId -> CurrentTime -> AuthHandler m Text
genUrl token tid currTime = do
    tm <- getRouteToParent
    render <- getUrlRender
    paramName <- tokenParamName
    let query = "?" <> paramName <> "=" <> tid <> ":" <> token <> ":" <> currTime
    return $ render (tm loginPostR) <> query


class YesodAuthPersist master => NoPasswordAuth master where
    -- | Route to a page that dispays a login form. This is not provided by
    -- the plugin.
    loginRoute :: master -> Route master

    -- | Route to which the user should be sent after entering an email
    -- address. This is not provided by the plugin.
    --
    -- __Note__: the user will not be authenticated when they reach the page.
    emailSentRoute :: master -> Route master

    -- | Send a login email.
    sendLoginEmail :: Email -- ^ The email to send to
                   -> Text  -- ^ The URL that will log the user in
                   -> AuthHandler master ()

    -- | Get a user by their email address. Used to determine if the user exists or not.
    getUserByEmail :: Email -> AuthHandler master (Maybe (AuthId master))

    -- | Get a Hash by a TokenId.
    --
    -- Invoked when the user returns to the site from an email. We don't know
    -- who the user is at this point as they may open the link from the email
    -- on another device or in another browser, so session data can't be used.
    -- Equally we do not want to pass the user's ID or email address in a URL
    -- if we don't have to, so instead we look up users by the 'TokenId' that
    -- we issued them earlier in the process.
    getEmailAndHashByTokenId :: TokenId -> AuthHandler master (Maybe (Email, Hash, AuthId master))

    -- | Update a user's login hash
    --
    -- This is also used to blank out the hash once the user has logged in, or
    -- can be used to prevent the user from logging in, so must accept a value
    -- of `Nothing`.
    --
    -- /It is recommended that the/ 'TokenId' /storage be enforced as unique/.
    -- For this reason, the token is not passed as a maybe, as some storage
    -- backends treat `NULL` values as the same.
    updateLoginHashForUser :: AuthId master -> Maybe Hash -> TokenId -> AuthHandler master ()

    -- | Create a new user with an email address and hash.
    newUserWithLoginHash :: Email -> Hash -> TokenId -> AuthHandler master ()

    -- | __Optional__ – return a custom token strength.
    --
    -- A token strength of @x@ equates to @2^x@ hash rounds.
    tokenStrength :: AuthHandler master Int
    tokenStrength = return 17

    -- | __Optional__ – return a custom token param name.
    tokenParamName :: AuthHandler master Text
    tokenParamName = return "tkn"

    settings :: AuthHandler master NoPasswordSettings

    {-# MINIMAL loginRoute
              , emailSentRoute
              , sendLoginEmail
              , getUserByEmail
              , getEmailAndHashByTokenId
              , updateLoginHashForUser
              , newUserWithLoginHash
              , settings #-}
