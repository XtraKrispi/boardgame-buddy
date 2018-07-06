{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Poll where

import Import

isActive :: Day -> Poll -> Bool
isActive currentDate (Poll _ startDate Nothing) =
  currentDate >= startDate
isActive currentDate (Poll _ startDate (Just endDate)) =
  currentDate >= startDate && currentDate <= endDate

getPollsR :: Handler Html
getPollsR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  polls <- runDB $ selectList [] [Asc PollStartDate]
  defaultLayout $ do
    setTitle "Boardgame Buddy | Polls"
    $(widgetFile "polls/polls")

getCreatePollR :: Handler Html
getCreatePollR = 
  defaultLayout $ do
    setTitle "Boardgame Buddy | New Poll"
    $(widgetFile "polls/editPoll")