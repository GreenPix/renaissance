{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Persist.Sqlite
import Control.Monad.Logger (runNoLoggingT)
import Auth.Token
import Auth.Token.Persistent
import Network.Wai.Handler.Warp
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.Client (BaseUrl(..), Scheme(..))

import Bz.Monad
import Bz.Server
import Bz.Model

main :: IO ()
main = do pool <- runNoLoggingT $ createSqlitePool "bz.sqlite" 10
          initAuthenticator pool
          runSqlPool (runMigration bzMigrateAuth) pool
          manager <- newManager defaultManagerSettings

          let conf = BzConfig pool manager (BaseUrl Http "localhost" 12000 "")

          run 11000 (bzd conf)
