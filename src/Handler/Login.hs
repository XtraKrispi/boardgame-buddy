{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Login where

import Import
import Data.Time.Calendar
import qualified Data.Text as T
import Text.Read
import qualified Components.Calendar as Cal
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Utils.GfyCatStyleUrls
import Control.Monad.Random hiding (forM_)
import Db.Polls
import Handler.Common
import Auth.NoPassword (EmailForm(..), loginPostR)

loginForm :: Form EmailForm
loginForm extra = do
  (emailRes, emailView) <- mreq emailField "" Nothing
  return (EmailForm <$> emailRes, $(widgetFile "login/loginForm"))

getUserLoginR :: Handler Html
getUserLoginR = do
  ((res, widget), enctype) <- runFormPost loginForm    
  authLayout $
    $(widgetFile "login/login")
