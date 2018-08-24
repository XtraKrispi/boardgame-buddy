{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Db.Polls where

import Import
import qualified Data.Text as T

getActivePolls :: DB [Entity Poll]
getActivePolls = selectList [ PollExpiryDate ==. Nothing ] [ Asc PollStartDate]

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

getPollForm :: T.Text -> DB (Maybe PollForm)
getPollForm friendlyUrl =
  (getBy $ UniquePollUrl friendlyUrl)
    >>= (maybe (return Nothing) $ \(Entity pollId Poll {..}) -> do
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

