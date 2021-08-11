{-# LANGUAGE OverloadedStrings #-}

module Resources
  ( Resources(..)
  , mkResources
  )
where

import           Control.Monad.Managed
import           Database.PostgreSQL.Resilient
import qualified Database.PostgreSQL.Simple    as P

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
                , P.connectPassword = "secret"
                , P.connectDatabase = "fts"
                }
 where
  logHandler :: String -> IO ()
  logHandler _ = return ()
