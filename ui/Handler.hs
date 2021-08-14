{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( eventHandler
  ) where

import           Control.Lens
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           HTTP.Posters                   ( ApiKey
                                                , fetchPoster
                                                )
import           Monomer
import           Services.Movies                ( Movies )
import           Types.Movie

searchMovies :: Maybe ApiKey -> Movies IO -> Text -> IO MoviesEvt
searchMovies apiKey service input = do
  m   <- toTitleText input & service ^. #findTitle
  res <- traverse (\x -> (`fromMovie` x) <$> fetchPoster apiKey x) m
  return $ MoviesSearchResult res

eventHandler
  :: Maybe ApiKey
  -> Movies IO
  -> WidgetEnv MoviesModel MoviesEvt
  -> WidgetNode MoviesModel MoviesEvt
  -> MoviesModel
  -> MoviesEvt
  -> [EventResponse MoviesModel MoviesEvt MoviesModel ()]
eventHandler apiKey service wenv _ model evt = case evt of
  MoviesInit -> [setFocusOnKey wenv "query"]
  MoviesSearch ->
    [ Model $ model & searching .~ True
    , Task $ searchMovies apiKey service (model ^. query)
    ]
  MoviesSearchResult resp ->
    [ Message "mainScroll" ScrollReset
    , Model $ model & searching .~ False & errorMsg .~ Nothing & movies .~ resp
    ]
  MoviesSearchError msg ->
    [Model $ model & searching .~ False & errorMsg ?~ msg & movies .~ []]
  MoviesShowDetails m -> [Model $ model & selected ?~ m]
  MoviesCloseDetails  -> [Model $ model & selected .~ Nothing]
  MoviesCloseError    -> [Model $ model & errorMsg .~ Nothing]
