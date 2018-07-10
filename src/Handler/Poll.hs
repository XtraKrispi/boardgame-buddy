{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Poll where

import Import
import Data.Time.Calendar
import qualified Data.Text as T
import Text.Read

data PollForm = PollForm {
   pollFormTitle          :: Text
  ,pollFormEffectiveDate  :: Day
  ,pollFormExpiryDate     :: Maybe Day
  ,pollFormApplicableDays :: [Day]
} deriving (Show)

isActive :: Day -> Poll -> Bool
isActive currentDate (Poll _ startDate Nothing Nothing _) =
  currentDate >= startDate
isActive _ (Poll _ _ _ (Just _) _) = False
isActive currentDate (Poll _ startDate (Just endDate) Nothing _) =
  currentDate >= startDate && currentDate <= endDate

convertToDays :: Text -> [Day]
convertToDays = fmap (read . T.unpack) . T.splitOn ","

convertFromDays :: [Day] -> Text
convertFromDays = T.concat . fmap (T.pack . showGregorian)

applicableDaysField :: Field Handler [Day]
applicableDaysField =
  customErrorMessage "Must pick at least one day!"
    $ convertField convertToDays convertFromDays hiddenField

getPollsR :: Handler Html
getPollsR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  polls <- runDB $ selectList [] [Asc PollStartDate]
  defaultLayout $ do
    setTitle "Boardgame Buddy | Polls"
    $(widgetFile "polls/polls")

pollForm :: Html -> MForm Handler (FormResult PollForm, Widget)
pollForm extra = do
  currentDay <- liftIO $ utctDay <$> getCurrentTime
  let bulmaControlFieldSettings = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("class", "input")]
        }

  (titleRes, titleView) <- mreq textField bulmaControlFieldSettings Nothing
  (effectiveDateRes, effectiveDateView) <- mreq dayField
                                                bulmaControlFieldSettings
                                                (Just currentDay)
  (expiryDateRes, expiryDateView) <- mopt dayField
                                          bulmaControlFieldSettings
                                          Nothing
  (applicableDaysRes, applicableDaysView) <- mreq applicableDaysField "" Nothing
  let pollFormRes =
        PollForm
          <$> titleRes
          <*> effectiveDateRes
          <*> expiryDateRes
          <*> applicableDaysRes
  return (pollFormRes, $(widgetFile "polls/pollForm"))

getCreatePollR :: Handler Html
getCreatePollR = do
  ((res, widget), enctype) <- runFormPost pollForm
  defaultLayout $ do
    setTitle "Boardgame Buddy | New Poll"
    $(widgetFile "polls/createPoll")

postCreatePollR :: Handler Html
postCreatePollR = do
  ((res, widget), enctype) <- runFormPost pollForm
  defaultLayout $ do
    setTitle "Boardgame Buddy | New Poll"
    $(widgetFile "polls/createPoll")
