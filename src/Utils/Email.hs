{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Email where

import Settings (MailSettings(..)) 
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL

loginEmailBody :: T.Text -> LT.Text
loginEmailBody url = 
    R.renderHtml $ H.div $ do
      H.p $ do
        H.text url

sendLoginEmail :: T.Text -> T.Text -> MailSettings -> IO (Either T.Text ())
sendLoginEmail emailAddress url MailSettings{..} = do
  doSMTPSSL mailHost $ \conn -> do
    authSucceed <- authenticate PLAIN mailUsername mailPassword conn
    if authSucceed
      then Right <$> sendMimeMail (T.unpack emailAddress) "XtraKrispi@gmail.com" "Board Game Buddy Login" (LT.pack "Html Content") (loginEmailBody url) [] conn
      else return . Left $ "Bad Auth"    

