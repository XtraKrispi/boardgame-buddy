{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Email where

import Import

getEmailSentR :: Handler Html
getEmailSentR = do
  authLayout $
    $(widgetFile "email/email")
