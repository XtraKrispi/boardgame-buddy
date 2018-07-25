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
  -> UserId
  -> DB (Key Poll, [Key PollAvailableDate], Key PollUser)
insertPoll poll' days userId = do
  pollKey     <- insert poll'
  pollDayKeys <- forM days $ \day -> do
    insert $ PollAvailableDate day pollKey
  pollUserKey <- insert $ PollUser userId pollKey True
  return (pollKey, pollDayKeys, pollUserKey)

getPollForm :: T.Text -> DB (Maybe PollForm)
getPollForm friendlyUrl =
  (getBy $ UniquePollUrl friendlyUrl)
    >>= (maybe (return Nothing) $ \(Entity pollId Poll {..}) -> do
          applicableDays <-
            fmap (pollAvailableDateDate . entityVal) <$> selectList
              [PollAvailableDatePollId ==. pollId]
              [Asc PollAvailableDateDate]
          entity <- selectList
            [PollUserPollId ==. pollId, PollUserIsPollCreator ==. True]
            [LimitTo 1]
          case entity of
            [Entity _ pollUser] -> do
              mUser <- get (pollUserUserId pollUser)
              case mUser of
                Nothing -> return Nothing
                Just _ ->
                  return $ Just $ PollForm
                    { pollFormTitle          = pollTitle
                    , pollFormEffectiveDate  = pollStartDate
                    , pollFormExpiryDate     = pollExpiryDate
                    , pollFormApplicableDays = applicableDays
                    }
            _ -> return Nothing          
        )
