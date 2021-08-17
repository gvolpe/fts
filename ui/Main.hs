{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                   ( view )
import           Control.Monad.Managed          ( with )
import           Data.Generics.Labels           ( )
import           Handler                        ( eventHandler )
import           Monomer
import           Resources
import           Services.Movies                ( mkMovies )
import           Types.Movie
import           UI.Movies                      ( moviesUI )
import           UI.Themes                      ( customDarkTheme )
import           Utils                          ( safeGetEnv )

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
