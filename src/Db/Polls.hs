{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Db.Polls where

import Import
import qualified Data.Text as T

getActivePolls :: DB [Entity Poll]
getActivePolls = selectList [ PollExpiryDate ==. Nothing, PollIsDeleted ==. False ] [ Asc PollStartDate]

insertPoll
  :: Poll
  -> [Day]
  -> UserId
  -> DB (Key Poll, [Key PollAvailableDate], Key PollUser)
insertPoll poll' days userId = do
  pollKey     <- insert poll'
  pollDayKeys <- forM days $ \day -> insert $ PollAvailableDate day pollKey
  pollUserKey <- insert $ PollUser userId pollKey
  return (pollKey, pollDayKeys, pollUserKey)

updatePoll
  :: Poll
  -> [Day]
  -> DB (Either String [Key PollAvailableDate])
updatePoll poll' days = do
  mEntity <- getBy $ UniquePollUrl (pollFriendlyUrl poll')
  case mEntity of
    Just (Entity pollKey _) -> do
      replace pollKey poll'
      deleteWhere [PollAvailableDatePollId ==. pollKey]
      dayKeys <- forM days $ \day -> insert $ PollAvailableDate day pollKey
      return $ Right dayKeys
    Nothing -> return (Left "Poll was not in the database")

getPollForm :: T.Text -> DB (Maybe PollForm)
getPollForm friendlyUrl =
  (getBy $ UniquePollUrl friendlyUrl)
    >>= (maybe (return Nothing) $ \(Entity pollId Poll {..}) ->
          if pollIsDeleted then
            return Nothing
          else do
            applicableDays <-
              fmap (pollAvailableDateDate . entityVal) <$> selectList
                [PollAvailableDatePollId ==. pollId]
                [Asc PollAvailableDateDate]
            return $ Just $ PollForm
              { pollFormTitle          = pollTitle
              , pollFormEffectiveDate  = pollStartDate
              , pollFormExpiryDate     = pollExpiryDate
              , pollFormApplicableDays = applicableDays
              })

deletePoll :: T.Text -> DB ()
deletePoll friendlyUrl = do
    mPoll <- getBy $ UniquePollUrl friendlyUrl
    case mPoll of
      Just (Entity pollId _) ->
        update pollId [PollIsDeleted =. True]
      Nothing -> return ()
