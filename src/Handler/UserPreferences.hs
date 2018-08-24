{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.UserPreferences where

import Import
import Utils.Message
import Db.Users
import qualified Text.Blaze.Html5 as H

data UserPreferencesForm = UserPreferencesForm
  { userPreferencesFormNickname :: Maybe Text
  , userPreferencesFormBggUsername :: Maybe Text
  }

userPreferencesForm :: User -> Form UserPreferencesForm
userPreferencesForm User{..} extra = do
  let backRoute = HomeR
  let bulmaControlFieldSettings = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("class", "input")]
        }

  (nicknameRes, nicknameView) <- mopt textField
                                      bulmaControlFieldSettings
                                      (return userNickname)
  (bggUsernameRes, bggUsernameView) <- mopt textField
                                      bulmaControlFieldSettings
                                      (return userBggUsername)
  let userPreferencesFormRes = UserPreferencesForm <$> nicknameRes <*> bggUsernameRes
  return
    ( userPreferencesFormRes
    , $(widgetFile "user-preferences/userPreferencesForm")
    )

getUserPreferencesR :: Handler Html
getUserPreferencesR = do
  mUser <- maybeAuthId >>= maybe (return Nothing) (runDB . getEntity)
  case mUser of
    Nothing -> permissionDenied "User cannot be found"
    Just (Entity _ user) -> do
      let mmsg = Nothing :: Maybe H.Html
      ((_, widget), enctype) <- runFormPost (userPreferencesForm user)
      defaultLayout $ do
        setTitle "Boardgame Buddy!"
        $(widgetFile "user-preferences/userPreferences")

postUserPreferencesR :: Handler Html
postUserPreferencesR = do
  mUser <- maybeAuthId >>= maybe (return Nothing) (runDB . getEntity)
  case mUser of
    Nothing -> permissionDenied "User cannot be found"
    Just (Entity userId user) -> do
      ((res, widget), enctype) <- runFormPost (userPreferencesForm user)
      case res of
        FormSuccess UserPreferencesForm{..} -> do
          runDB $ updateUser
                  userId
                  (user { userNickname = userPreferencesFormNickname, userBggUsername = userPreferencesFormBggUsername })
          setMessage $ convertMessage
              (Message "User preferences were updated successfully!"
                       MessageSuccess
              )
        _ -> setMessage $ convertMessage
          (Message "There was a problem with your request. Please try again."
                   MessageError
          )
    --_ -> setMessage $ convertMessage (Message "This is a test" MessageInfo)
      mmsg <- getMessage
      defaultLayout $ do
        setTitle "Boardgame Buddy | User Preferences"
        $(widgetFile "user-preferences/userPreferences")
