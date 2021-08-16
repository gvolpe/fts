{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( eventHandler
  ) where

import           Control.Lens
import           Data.Foldable                  ( traverse_ )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           HTTP.Posters                   ( ApiKey
                                                , fetchPoster
                                                )
import           Monomer
import           Services.Movies                ( Movies )
import           Types.Movie

searchMovies
  :: Maybe ApiKey -> Movies IO -> Text -> (MoviesEvt -> IO ()) -> IO ()
searchMovies apiKey service input sendMsg =
  toTitleText input & service ^. #findTitle >>= \case
    [] -> sendMsg $ MoviesSearchError "No hits"
    xs -> traverse_ (g . f) xs
 where
  f x = (`fromMovie` x) <$> fetchPoster apiKey x
  g x = x >>= \s -> s <$ sendMsg (MoviesSearchResult [s])

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
    [ Model $ model & searching .~ True & movies .~ []
    , Producer $ searchMovies apiKey service (model ^. query)
    , Task $ return MoviesInit
    ]
  MoviesSearchResult resp ->
    [ Message "mainScroll" ScrollReset
    , Model
      $  model
      &  searching
      .~ False
      &  errorMsg
      .~ Nothing
      &  movies
      %~ (++ resp)
    ]
  MoviesSearchError msg ->
    [Model $ model & searching .~ False & errorMsg ?~ msg & movies .~ []]
  MoviesShowDetails m -> [Model $ model & selected ?~ m]
  MoviesCloseDetails  -> [Model $ model & selected .~ Nothing]
  MoviesCloseError    -> [Model $ model & errorMsg .~ Nothing]
