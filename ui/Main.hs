{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Managed          ( with )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           EventHandler                   ( eventHandler )
import           Monomer
import           Resources
import           Services.Movies                ( mkMovies )
import           System.Environment             ( getEnv )
import           Types.Movie
import           UI.Movies                      ( moviesUI )
import           UI.Themes                      ( customDarkTheme )

main :: IO ()
main = with managedMovies $ \service -> do
  apiKey <- safeGetEnv "TMDB_API_KEY"
  startApp initModel (eventHandler apiKey service) moviesUI config
 where
  managedMovies = mkMovies . view #postgres <$> mkResources
  config =
    [ appWindowTitle "🎬 Movie search"
    , appTheme customDarkTheme
    , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
    , appFontDef "Medium"  "./assets/fonts/Roboto-Medium.ttf"
    , appInitEvent MoviesInit
    ]
  initModel = MoviesModel "" False Nothing [] Nothing

safeGetEnv :: String -> IO (Maybe Text)
safeGetEnv var = (Just . T.pack <$> getEnv var) `orElse` pure Nothing

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

orElse :: IO a -> IO a -> IO a
orElse fa fb = catchAny fa (const fb)
