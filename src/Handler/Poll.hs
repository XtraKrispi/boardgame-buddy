{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Poll where

import Import

getPollsR :: Handler Html
getPollsR = 
  defaultLayout $ do
    setTitle "Boardgame Buddy | Polls"
    $(widgetFile "polls/polls")