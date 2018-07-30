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
import Control.Monad.Random hiding (forM_)
import Db.Polls

data MessageType = MessageSuccess | MessageInfo | MessageWarning | MessageError

data Message = Message {
   message :: Text
  ,messageType :: MessageType
}

instance B.ToMarkup Message where
  toMarkup (Message msg t) = do
    let class_ = case t of
                  MessageSuccess -> "is-primary"
                  MessageInfo -> "is-info"
                  MessageWarning -> "is-warning"
                  MessageError -> "is-danger"
    H.div B.! H.class_ ("notification " <> class_ <> " notification-message") $ do
      H.text msg

convertToPoll :: MonadRandom m => PollForm -> m (Poll, [Day])
convertToPoll PollForm {..} = do
  pollFriendlyUrl <- generate (UrlGenerationConfig "-" Lowercase 2)
  return (Poll {..}, pollFormApplicableDays)
 where
  pollTitle      = pollFormTitle
  pollStartDate  = pollFormEffectiveDate
  pollExpiryDate = pollFormExpiryDate
  pollClosedDate = Nothing

convertMessage :: Message -> Html
convertMessage = B.toMarkup

applicableDaysId :: Text
applicableDaysId = "applicableDays"

isActive :: Day -> Poll -> Bool
isActive currentDate (Poll _ startDate Nothing Nothing _) =
  currentDate >= startDate
isActive _ (Poll _ _ _ (Just _) _) = False
isActive currentDate (Poll _ startDate (Just endDate) Nothing _) =
  currentDate >= startDate && currentDate <= endDate

convertToDays :: Text -> [Day]
convertToDays = fmap (read . T.unpack) . T.splitOn ","

convertFromDays :: [Day] -> Text
convertFromDays = T.concat . intersperse "," . fmap (T.pack . showGregorian)

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

pollForm :: Form PollForm
pollForm extra = do
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

  (titleRes, titleView) <- mreq textField bulmaControlFieldSettings Nothing
  (effectiveDateRes, effectiveDateView) <- mreq dayField
                                                bulmaDatePickerFieldSettings
                                                (Just currentDay)
  (expiryDateRes, expiryDateView) <- mopt dayField
                                          bulmaDatePickerFieldSettings
                                          Nothing
  (applicableDaysRes, applicableDaysView) <- mreq applicableDaysField
                                                  applicableDaysFieldSettings
                                                  Nothing
  let pollFormRes =
        PollForm
          <$> titleRes
          <*> effectiveDateRes
          <*> expiryDateRes
          <*> applicableDaysRes
  return (pollFormRes, $(widgetFile "polls/pollForm"))

getCreatePollR :: Handler Html
getCreatePollR = do
  mmsg                   <- getMessage
  ((_, widget), enctype) <- runFormPost pollForm
  defaultLayout $ do
    setTitle "Boardgame Buddy | New Poll"
    $(widgetFile "polls/createPoll")

postCreatePollR :: Handler Html
postCreatePollR = do
  ((res, widget), enctype) <- runFormPost pollForm
  case res of
    FormSuccess formData -> do
      (poll', days) <- liftIO . convertToPoll $ formData
      mUserId       <- maybeAuthId
      case mUserId of
        Nothing ->
          setMessage $ convertMessage (Message "This is a test" MessageError)
        Just userId -> do
          _ <- runDB $ insertPoll poll' days userId
          setMessage $ convertMessage
            (Message "The poll was created successfully!" MessageSuccess)
          redirect $ EditPollR $ pollFriendlyUrl poll'
    FormFailure _ ->
      setMessage $ convertMessage (Message "This is a test" MessageError)
    _ -> setMessage $ convertMessage (Message "This is a test" MessageInfo)
  mmsg <- getMessage
  defaultLayout $ do
    setTitle "Boardgame Buddy | New Poll"
    $(widgetFile "polls/createPoll")

getEditPollR :: T.Text -> Handler Html
getEditPollR friendlyUrl = do
  mPollForm <- runDB $ getPollForm friendlyUrl
  case mPollForm of
    Nothing            -> notFound
    Just PollForm {..} -> defaultLayout $ do
      setTitle . H.text $ "Boardgame Buddy | Edit " <> pollFormTitle

getViewPollR :: T.Text -> Handler Html
getViewPollR friendlyUrl = do
  mPoll <- runDB $ do
    getBy $ UniquePollUrl friendlyUrl
  case mPoll of
    Nothing                   -> notFound
    Just (Entity _ Poll {..}) -> defaultLayout $ do
      setTitle $ H.text $ "Boardgame Buddy | " <> pollTitle
