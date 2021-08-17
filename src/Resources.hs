{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Resources
  ( Resources(..)
  , mkResources
  ) where

import           Control.Monad.Managed
import           Database.PostgreSQL.Resilient
import qualified Database.PostgreSQL.Simple    as P
import           GHC.Generics                   ( Generic )

newtype Resources = Res
  { postgres :: ResilientConnection IO
  } deriving Generic

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
