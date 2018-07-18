{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Db.Users where

import Import.NoFoundation
import qualified Data.Text as T

getUser :: (MonadIO m) => T.Text -> ReaderT SqlBackend m (Maybe (Entity User))
getUser email = getBy $ UniqueEmail email