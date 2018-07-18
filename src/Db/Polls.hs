{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Db.Polls where

import Import
import qualified Data.Text as T

insertPoll
  :: Poll
  -> [Day]
  -> T.Text
  -> DB (Key Poll, [Key PollAvailableDate], Key PollUser)
insertPoll poll days username = do
  pollKey     <- insert poll
  pollDayKeys <- forM days $ \day -> do
    insert $ PollAvailableDate day pollKey
  pollUserKey <- insert $ PollUser username pollKey True
  return (pollKey, pollDayKeys, pollUserKey)

getPollForm :: T.Text -> DB (Maybe PollForm)
getPollForm friendlyUrl =
  (getBy $ UniquePollUrl friendlyUrl)
    >>= (maybe (return Nothing) $ \(Entity pollId Poll {..}) -> do
          applicableDays <-
            fmap (pollAvailableDateDate . entityVal) <$> selectList
              [PollAvailableDatePollId ==. pollId]
              [Asc PollAvailableDateDate]
          [Entity userId user] <- selectList
            [PollUserPollId ==. pollId, PollUserIsPollCreator ==. True]
            [LimitTo 1]
          return $ Just $ PollForm
            { pollFormTitle          = pollTitle
            , pollFormUsername       = pollUserUsername user
            , pollFormEffectiveDate  = pollStartDate
            , pollFormExpiryDate     = pollExpiryDate
            , pollFormApplicableDays = applicableDays
            }
        )
