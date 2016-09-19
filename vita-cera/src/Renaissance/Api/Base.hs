{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Renaissance.Api.Base
  ( GetPaginated
  ) where

import Servant.API

type GetPaginated f t = QueryParam "page" Int
                     :> QueryParam "page_size" Int
                     :> Get f [t]
