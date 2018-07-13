{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Calendar where

import Import

mkWidget :: Text -> Widget
mkWidget fieldId = do
  $(widgetFile "components/calendar/calendar")