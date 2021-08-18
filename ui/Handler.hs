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
    xs -> case apiKey of
      (Just k) -> traverse_ (f k) xs
      Nothing  -> g xs
 where
  f k x = do
    s <- (`fromMovie` x) <$> fetchPoster k x
    s <$ sendMsg (MoviesSearchResult [s])
  g xs = do
    putStrLn "No value set for TMDB_API_KEY, cannot fetch movie posters"
    sendMsg $ MoviesSearchResult (fromMovie Nothing <$> xs)

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
