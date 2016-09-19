{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module UUID where

import Data.UUID (UUID, toText, fromText)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH

instance PersistField UUID  where
  toPersistValue = PersistText . toText

  fromPersistValue (PersistText tx) = case fromText tx of
                                        Just uuid -> Right uuid
                                        _         -> Left "Not a valid UUID"

  fromPersistValue _                = Left "UUID must be converted from Text"

instance PersistFieldSql UUID where
  sqlType _ = SqlString
