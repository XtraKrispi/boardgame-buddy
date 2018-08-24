{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Login where

import Import
import Auth.NoPassword (EmailForm(..), loginPostR)

loginForm :: Form EmailForm
loginForm extra = do
  let emailSettings = FieldSettings
                        { fsLabel   = ""
                        , fsTooltip = Nothing
                        , fsId      = Nothing
                        , fsName    = Nothing
                        , fsAttrs   = [("class", "input is-large")
                                      ,("placeholder", "Your Email")]
                        }
  (emailRes, emailView) <- mreq emailField emailSettings Nothing
  return (EmailForm <$> emailRes, $(widgetFile "login/loginForm"))

getUserLoginR :: Handler Html
getUserLoginR = do
  ((_, widget), enctype) <- runFormPost loginForm
  authLayout $(widgetFile "login/login")
