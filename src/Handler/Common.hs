{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

loggedInLayout :: Widget -> Handler Html                    
loggedInLayout widget = do
  master <- getYesod
  mmsg <- getMessage

  mcurrentRoute <- getCurrentRoute

  -- Define the menu items of the header.
  let menuItems =
          [ NavbarLeft $ MenuItem
              { menuItemLabel = "Home"
              , menuItemRoute = HomeR
              , menuItemAccessCallback = True
              , menuItemRelatedRoutes = []
              }
          , NavbarLeft $ MenuItem
              { menuItemLabel = "Polls"
              , menuItemRoute = PollsR
              , menuItemAccessCallback = True
              , menuItemRelatedRoutes = [CreatePollR]
              }
          , NavbarLeft $ MenuItem
              { menuItemLabel = "Game Nights"
              , menuItemRoute = GameNightsR
              , menuItemAccessCallback = True
              , menuItemRelatedRoutes = []
              }
          ]

  let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
  let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

  let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
  let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
  layout $(widgetFile "default-layout")                