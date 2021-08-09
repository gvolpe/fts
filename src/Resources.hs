{-# LANGUAGE OverloadedStrings #-}

module Resources
  ( Resources(..)
  , mkResources
  )
where

import           Control.Monad.Catch
import           Control.Monad.Managed
import qualified Data.Text                     as T
import           Database.PostgreSQL.Resilient
import qualified Database.PostgreSQL.Simple    as P
import           Effects.Logger

data Resources = Res
  { postgres :: ResilientConnection IO
  }

mkResources :: Managed Resources
mkResources = Res <$> postgresResource

postgresResource :: Managed (ResilientConnection IO)
postgresResource = managed $ withResilientConnection
  defaultResilientSettings
  logHandler
  P.ConnectInfo { P.connectHost     = "localhost"
                , P.connectPort     = 5432
                , P.connectUser     = "postgres"
                , P.connectPassword = "my-password"
                , P.connectDatabase = "store"
                }
 where
  logHandler :: String -> IO ()
  logHandler = logInfo . T.pack
