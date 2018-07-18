{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.GameNight where

import Import
import Handler.Common

getGameNightsR :: Handler Html
getGameNightsR =
  loggedInLayout $ do
    setTitle "Boardgame Buddy | Game Nights"
    $(widgetFile "gameNight/gameNights")