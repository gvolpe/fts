{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.MovieImage where

import           Data.Text                      ( Text )
import           Monomer
import           Types.Movie

movieImage :: Maybe Text -> WidgetNode s MoviesEvt
movieImage = maybe filler coverImg where
  baseUrl = "https://www.themoviedb.org/t/p/w1280"
  coverImg i = image_ (baseUrl <> i) [fitHeight, alignRight]
