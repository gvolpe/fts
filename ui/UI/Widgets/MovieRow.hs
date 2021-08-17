{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.MovieRow where

import           Control.Lens
import           Data.Default                   ( def )
import           Data.Maybe                     ( fromMaybe )
import           Monomer
import qualified Monomer.Lens                  as L
import           TextShow                       ( showt )
import           Types.Movie
import           UI.Widgets.MovieImage          ( movieImage )

movieRow :: MoviesWenv -> MovieDTO -> MoviesNode
movieRow wenv m = row where
  rowBgColor  = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  publishYear = maybe "Unknown" showt (m ^. year)
  genre'      = fromMaybe "Unknown" (m ^. genre)

  rowContent  = hstack
    [ vstack
      [ label_ (m ^. titleName) [resizeFactor 1]
        `styleBasic` [textFont "Medium", textSize 16]
      , spacer
      , label_ genre' [resizeFactor 1] `styleBasic` [textSize 14]
      ]
    , filler
    , vstack
      [ label publishYear `styleBasic` [height 100, width 50, textSize 14]
      , spacer
      ]
    , movieImage (m ^. poster) `styleBasic` [width 35]
    ]

  row = box_ cfg content `styleBasic` [padding 10, paddingT 0]   where
    cfg = [expandContent, onClick (MoviesShowDetails m)]
    content =
      rowContent
        `styleBasic` [height 80, padding 20, radius 5]
        `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
