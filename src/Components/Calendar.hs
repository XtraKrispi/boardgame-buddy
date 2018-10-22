{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Calendar where

import Import

mkWidget :: Text -> Widget
mkWidget fieldId = do
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/date-fns/1.29.0/date_fns.min.js"
  $(widgetFile "components/calendar/calendar")
