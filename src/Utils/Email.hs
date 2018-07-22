{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Email where

import Settings (MailSettings(..)) 
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import qualified Text.Blaze.Html.Renderer.Text as R
import Network.HaskellNet.SMTP
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL

loginEmailBody :: T.Text -> T.Text -> LT.Text
loginEmailBody verKey verUrl = 
    R.renderHtml $ H.div $ do
      H.p $ do
        H.text verKey
      H.p $ do
        H.text verUrl

sendLoginEmail :: MailSettings -> T.Text -> T.Text -> T.Text -> IO ()
sendLoginEmail MailSettings{..} emailAddress verKey verUrl = do
  doSMTPSSL mailHost $ \conn -> do
    authSucceed <- authenticate PLAIN mailUsername mailPassword conn
    if authSucceed
      then sendMimeMail (T.unpack emailAddress) "noreply@boardgamebuddy.com" "Board Game Buddy Login" (LT.pack "Html Content") (loginEmailBody verKey verUrl) [] conn
      else fail "Bad Auth"    

