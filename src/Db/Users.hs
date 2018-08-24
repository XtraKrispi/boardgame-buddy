{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Db.Users where

import Import.NoFoundation
import qualified Data.Text as T

getUser :: (MonadIO m) => UserId -> ReaderT SqlBackend m (Maybe User)
getUser = get

getUserByEmail :: (MonadIO m) => T.Text -> ReaderT SqlBackend m (Maybe (Entity User))
getUserByEmail = getBy . UniqueEmail

getUserByTokenId :: (MonadIO m) => T.Text -> ReaderT SqlBackend m (Maybe (Entity User))
getUserByTokenId = getBy . UniqueToken

updateUser :: (MonadIO m) => UserId -> User -> ReaderT SqlBackend m ()
updateUser = replace

createUser :: (MonadIO m) => User -> ReaderT SqlBackend m (UserId)
createUser = insert
