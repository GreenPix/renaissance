{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Typeable
import GHC.Int
import Auth.Token
import Data.Proxy
import Data.Token
import Data.Text (Text)
import Options.Generic
import Servant.API
import Servant.PureScript
import Servant.Auth.Token.Api
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes
import qualified Data.Text.IO as T
import Language.PureScript.Bridge.TypeInfo
import Language.PureScript.Bridge.TypeParameters

import Renaissance.Api.Bz

-- The token bridge is necessary because the implementation of FromJSON and
-- ToJSON are not standard. Therefore, we shadow the type with the String
-- type.
tokenBridge :: BridgePart
tokenBridge = do
    typeName ^== "Token"
    return psString

bsBridge :: BridgePart
bsBridge = do
    typeName ^== "ByteString"
    return psString

int64Bridge :: BridgePart
int64Bridge = do
    typeName ^== "Int64"
    return psInt

utcBridge :: BridgePart
utcBridge = do
    typeName ^== "UTCTime"
    return psString

data Command =
    Command { bz :: Maybe FilePath }
  deriving (Generic, Show)

instance ParseRecord Command

data RenaissanceBridge
renaissanceBridge = tokenBridge <|> bsBridge <|> int64Bridge <|> utcBridge <|> defaultBridge

instance HasBridge RenaissanceBridge where
    languageBridge _ = buildBridge renaissanceBridge

bzApi :: Proxy ForeignBzApi
bzApi = Proxy

bzTypes :: [SumType 'Haskell]
bzTypes = [ mkSumType (Proxy :: Proxy WhoAmIResponse)
          , mkSumType (Proxy :: Proxy TokenGetRouteBody)
          , mkSumType (Proxy :: Proxy PostTokenRefreshReq)
          , mkSumType (Proxy :: Proxy Access)
          , mkSumType (Proxy :: Proxy Refresh)
          , mkSumType (Proxy :: Proxy (EphemeralToken A))
          , mkSumType (Proxy :: Proxy AccessGrant)
          ]

main :: IO ()
main = do
    cmd <- getRecord "ogma-cli" :: IO Command

    gen (bz cmd) bzApi
  where
    -- gen :: Maybe FilePath -> Proxy api -> IO ()
    gen (Just fp) api = do
        writeAPIModule fp (Proxy :: Proxy RenaissanceBridge) api
        writePSTypes fp (buildBridge renaissanceBridge) bzTypes
    gen _ _ = return ()
