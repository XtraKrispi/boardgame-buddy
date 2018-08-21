{-# LANGUAGE OverloadedStrings #-}

module Utils.Message (MessageType(..), Message(..), convertMessage) where
import Data.Text
import Data.Semigroup ((<>))
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H

data MessageType = MessageSuccess | MessageInfo | MessageWarning | MessageError

data Message = Message
  { message     :: Text
   ,messageType :: MessageType
  }

instance B.ToMarkup Message where
  toMarkup (Message msg t) = do
    let class_ = case t of
                  MessageSuccess -> "is-primary"
                  MessageInfo -> "is-info"
                  MessageWarning -> "is-warning"
                  MessageError -> "is-danger"
    H.div B.! H.class_ ("notification " <> class_ <> " notification-message") $
      H.text msg

convertMessage :: Message -> H.Html
convertMessage = B.toMarkup
