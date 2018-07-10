{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Calendar where

import Import

mkWidget :: [Day] -> Widget
mkWidget selectedDays = do
  $(widgetFile "components/calendar/calendar")