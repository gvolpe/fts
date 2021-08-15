module Utils where

import           Control.Exception
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Environment             ( getEnv )

safeGetEnv :: String -> IO (Maybe Text)
safeGetEnv var = (Just . T.pack <$> getEnv var) `orElse` pure Nothing

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

orElse :: IO a -> IO a -> IO a
orElse fa fb = catchAny fa (const fb)
