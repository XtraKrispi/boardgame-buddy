{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Poll where

import Import
import Data.Time.Calendar
import qualified Data.Text as T
import Text.Read
import qualified Components.Calendar as Cal
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Utils.GfyCatStyleUrls
import Control.Monad.Random
import Db.Polls
import Utils.Message


convertToPoll :: MonadRandom m => UserId -> PollForm -> m (Poll, [Day])
convertToPoll userId PollForm {..} = do
  pollFriendlyUrl <- generate (UrlGenerationConfig "-" Lowercase 2)
  return (Poll {..}, pollFormApplicableDays)
 where
  pollTitle           = pollFormTitle
  pollStartDate       = pollFormEffectiveDate
  pollExpiryDate      = pollFormExpiryDate
  pollClosedDate      = Nothing
  pollCreatedByUserId = userId
  pollIsDeleted       = False

applicableDaysId :: Text
applicableDaysId = "applicableDays"

isActive :: Day -> Poll -> Bool
isActive currentDate (Poll _ startDate Nothing Nothing _ _ False) =
  currentDate >= startDate
isActive _ (Poll _ _ _ (Just _) _ _ _) = False
isActive currentDate (Poll _ startDate (Just endDate) Nothing _ _ False) =
  currentDate >= startDate && currentDate <= endDate
isActive _ (Poll _ _ _ _ _ _ True) = False

convertToDays :: Text -> [Day]
convertToDays = fmap (read . T.unpack) . T.splitOn ","

convertFromDays :: [Day] -> Text
convertFromDays = T.concat . intersperse "," . fmap (T.pack . showGregorian)

applicableDaysField :: Field Handler [Day]
applicableDaysField =
  customErrorMessage "Must pick at least one day!"
    $ convertField convertToDays convertFromDays hiddenField

pollForm :: Maybe PollForm -> Form PollForm
pollForm mForm extra = do
  let backRoute      = PollsR
  let calendarWidget = Cal.mkWidget applicableDaysId
  currentDay <- liftIO $ utctDay <$> getCurrentTime
  let bulmaControlFieldSettings = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("class", "input")]
        }

  let bulmaDatePickerFieldSettings = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("class", "input date-picker")]
        }

  let applicableDaysFieldSettings = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Just applicableDaysId
        , fsName    = Nothing
        , fsAttrs   = []
        }

  (titleRes, titleView) <- mreq textField bulmaControlFieldSettings (pollFormTitle <$> mForm)
  (effectiveDateRes, effectiveDateView) <- mreq dayField
                                                bulmaDatePickerFieldSettings
                                                (Just $ maybe currentDay pollFormEffectiveDate mForm)
  (expiryDateRes, expiryDateView) <- mopt dayField
                                          bulmaDatePickerFieldSettings
                                          (pollFormExpiryDate <$> mForm)
  (applicableDaysRes, applicableDaysView) <- mreq applicableDaysField
                                                  applicableDaysFieldSettings
                                                  (pollFormApplicableDays <$> mForm)
  let pollFormRes =
        PollForm
          <$> titleRes
          <*> effectiveDateRes
          <*> expiryDateRes
          <*> applicableDaysRes
  return (pollFormRes, $(widgetFile "polls/pollForm"))

getPollsR :: Handler Html
getPollsR = do
  mUser <- maybeAuthId
  polls <- maybe
    (pure [])
    (\userId ->
      runDB
        $   fmap
              (\dbPoll@(Entity _ poll') ->
                (dbPoll, pollCreatedByUserId poll' == userId)
              )
        <$> getActivePolls
    )
    mUser
  today <- liftIO $ utctDay <$> getCurrentTime
  defaultLayout $ do
    setTitle "Boardgame Buddy | Polls"
    $(widgetFile "polls/polls")

getCreatePollR :: Handler Html
getCreatePollR = do
  mmsg                   <- getMessage
  ((_, widget), enctype) <- runFormPost (pollForm Nothing)
  defaultLayout $ do
    setTitle "Boardgame Buddy | New Poll"
    let pageTitle = "Create Poll" :: T.Text
    $(widgetFile "polls/editPoll")

postCreatePollR :: Handler Html
postCreatePollR = do
  ((res, widget), enctype) <- runFormPost (pollForm Nothing)
  case res of
    FormSuccess formData -> do
      mUserId <- maybeAuthId
      case mUserId of
        Nothing ->
          setMessage $ convertMessage (Message "This is a test" MessageError)
        Just userId -> do
          (poll', days) <- liftIO . convertToPoll userId $ formData
          _             <- runDB $ insertPoll poll' days userId
          redirect $ ViewPollR $ pollFriendlyUrl poll'
    FormFailure _ ->
      setMessage $ convertMessage (Message "This is a test" MessageError)
    _ -> setMessage $ convertMessage (Message "This is a test" MessageInfo)
  mmsg <- getMessage
  defaultLayout $ do
    setTitle "Boardgame Buddy | New Poll"
    let pageTitle = "Create Poll" :: T.Text
    $(widgetFile "polls/editPoll")

getEditPollR :: T.Text -> Handler Html
getEditPollR friendlyUrl = do
  mPollForm <- runDB $ getPollForm friendlyUrl
  case mPollForm of
    Nothing -> notFound
    Just form -> do
      mmsg                   <- getMessage
      ((_, widget), enctype) <- runFormPost (pollForm (Just form))
      defaultLayout $ do
        setTitle "Boardgame Buddy | Edit Poll"
        let pageTitle = "Edit Poll" :: T.Text
        $(widgetFile "polls/editPoll")

postEditPollR :: Handler Html
postEditPollR = do
  ((res, widget), enctype) <- runFormPost (pollForm Nothing)
  case res of
    FormSuccess formData -> do
      mUserId <- maybeAuthId
      case mUserId of
        Nothing ->
          setMessage $ convertMessage (Message "This is a test" MessageError)
        Just userId -> do
          (poll', days) <- liftIO . convertToPoll userId $ formData
          _             <- runDB $ insertPoll poll' days userId
          redirect $ ViewPollR $ pollFriendlyUrl poll'
    FormFailure _ ->
      setMessage $ convertMessage (Message "This is a test" MessageError)
    _ -> setMessage $ convertMessage (Message "This is a test" MessageInfo)
  mmsg <- getMessage
  defaultLayout $ do
    setTitle "Boardgame Buddy | Edit Poll"
    let pageTitle = "Edit Poll" :: T.Text
    $(widgetFile "polls/editPoll")

getViewPollR :: T.Text -> Handler Html
getViewPollR friendlyUrl = do
  mPoll <- runDB $ getBy $ UniquePollUrl friendlyUrl
  case mPoll of
    Nothing                   -> notFound
    Just (Entity _ Poll {..}) -> defaultLayout $ do
      setTitle $ H.text $ "Boardgame Buddy | " <> pollTitle
      $(widgetFile "polls/viewPoll")

getDeletePollR :: T.Text -> Handler Html
getDeletePollR friendlyUrl = do
  runDB $ deletePoll friendlyUrl
  redirect PollsR
