{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.GameNight where

import Import

getGameNightsR :: Handler Html
getGameNightsR =
  defaultLayout $ do
    setTitle "Boardgame Buddy | Game Nights"
    $(widgetFile "gameNight/gameNights")