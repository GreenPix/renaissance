{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runNoLoggingT)
import Network.Wai.Handler.Warp (run)

import Gjanajo.Monad
import Gjanajo.Server

main :: IO ()
main = do pool <- runNoLoggingT $ createSqlitePool "gjanajo.sqlite" 10

          let conf = GjanajoConfig pool

          run 8081 (gjanajod conf)
