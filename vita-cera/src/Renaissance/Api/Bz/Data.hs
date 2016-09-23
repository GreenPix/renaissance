{-# LANGUAGE DeriveGeneric #-}

module Renaissance.Api.Bz.Data where

import Data.Token (fromText, Token)
import Data.Token.Aeson
import GHC.Generics
import Data.Aeson
import Data.UUID.Aeson
import Data.UUID (UUID)
import Data.ByteString.Conversion.From

data Authorization
type AuthorizationToken = Token Authorization

instance FromByteString (Token a) where
    parser = do txt <- parser
                return $ fromText txt


data TokenGetRouteBody = TokenGetRouteBody
                       { authorizationToken :: AuthorizationToken }
  deriving (Generic)

instance FromJSON TokenGetRouteBody
instance ToJSON TokenGetRouteBody
  
data WhoAmIResponse = WhoAmIResponse
                    { id :: UUID }
  deriving (Generic)

instance FromJSON WhoAmIResponse
instance ToJSON WhoAmIResponse
