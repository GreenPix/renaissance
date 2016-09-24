{-|
Module: Renaissance.Api.Base
Copyright: (c) GreenPix, 2016
License: MIT
Stability: experimental

This module is intended to provide a set of definitions the Renaissance project
APIs can reuse at will.
-}

{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Renaissance.Api.Base
  ( GetPaginated
  ) where

import Servant.API

-- | An endpoint using GetPaginated returns a list of @t@ objects. The
-- request can be parameterized using the @page@ and @page_size@ query
-- parameters. It is up to the api implementation to choose a default value
-- for those two parameters, as they are optional.
type GetPaginated f t = QueryParam "page" Int
                     :> QueryParam "page_size" Int
                     :> Get f [t]
