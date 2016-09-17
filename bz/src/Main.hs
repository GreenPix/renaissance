{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Persist.Sqlite
import Control.Monad.Logger (runNoLoggingT)
import Auth.Token
import Auth.Token.Persistent
import Network.Wai.Handler.Warp

import Bz.Monad
import Bz.Server

main :: IO ()
main = do pool <- runNoLoggingT $ createSqlitePool "db.sqlite" 10
          initAuthenticator pool

          let conf = BzConfig pool

          run 8080 (bzd conf)
