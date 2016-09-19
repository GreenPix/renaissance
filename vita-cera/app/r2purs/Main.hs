{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens ((&), (.~))
import Control.Monad (when, void)
import Data.List (nub)
import Data.Typeable
import GHC.Int
import Auth.Token
import Data.Proxy
import Data.Token
import Data.Text (Text, append)
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
import Renaissance.Api.Gjanajo
import qualified Renaissance.Api.Gjanajo.Data.Account as A
import qualified Renaissance.Api.Gjanajo.Data.Character as C

-- The token bridge is necessary because the implementation of FromJSON and
-- ToJSON are not standard. Therefore, we shadow the type with the String
-- type. Same for UUID
tokenBridge :: BridgePart
tokenBridge = do
    typeName ^== "Token"
    return psString

uuidBridge :: BridgePart
uuidBridge = do
    typeName ^== "UUID"
    return psString

utcBridge :: BridgePart
utcBridge = do
    typeName ^== "UTCTime"
    return psString

data Command =
    Command { bz      :: Bool
            , gjanajo :: Bool
            , to      :: FilePath }
  deriving (Generic, Show)

instance ParseRecord Command

data RenaissanceBridge
renaissanceBridge = uuidBridge <|> tokenBridge <|> utcBridge <|> defaultBridge

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

gjanajoApi :: Proxy ForeignGjanajoApi
gjanajoApi = Proxy

gjanajoTypes :: [SumType 'Haskell]
gjanajoTypes = [ mkSumType (Proxy :: Proxy A.AccountInformation)
               , mkSumType (Proxy :: Proxy C.CharacterInformation)
               , mkSumType (Proxy :: Proxy C.Statistics)
               , mkSumType (Proxy :: Proxy NoContent)
               ]

main :: IO ()
main = do
    cmd <- getRecord "ogma-cli" :: IO Command

    let types = nub $ (if bz cmd then bzTypes else [])            ++
                      (if gjanajo cmd then gjanajoTypes else [])

    writePSTypes (to cmd) (buildBridge renaissanceBridge) types

    when (bz cmd) (void $ gen (to cmd) bzApi "BzApi")
    when (gjanajo cmd) (void $ gen (to cmd) gjanajoApi "GjanajoApi")
  where
    -- gen :: FilePath -> Proxy api -> name -> IO ()
    gen fp api name = do
        writeAPIModuleWithSettings (defaultSettings & apiModuleName .~ "Gen" `append` name) fp (Proxy :: Proxy RenaissanceBridge) api
