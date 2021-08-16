{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.MovieDetail where

import           Control.Lens
import           Data.List                      ( intercalate )
import           Data.Maybe
import qualified Data.Text                     as T
import           Monomer
import           TextShow
import           Types.Movie
import           UI.Widgets.MovieImage          ( movieImage )

movieDetail :: MovieDTO -> WidgetNode s MoviesEvt
movieDetail m = content `styleBasic` [minWidth 600, paddingH 20] where
  hasPoster   = isJust (m ^. poster)
  synopsis'   = fromMaybe "" (m ^. synopsis)
  publishYear = maybe "Unknown" showt (m ^. year)
  genre'      = fromMaybe "Unknown" (m ^. genre)
  actors'     = T.filter (/= '"') . showt $ intercalate ", " (m ^. actors)

  shortLabel value = label value `styleBasic` [textFont "Medium", textTop]
  longLabel value = label_ value [multiline, ellipsis, trimSpaces]

  content =
    hstack
      . concat
      $ [ [ vstack
              [ longLabel (m ^. titleName)
                `styleBasic` [textSize 20, textFont "Medium"]
              , spacer
              , shortLabel publishYear `styleBasic` [textSize 16]
              , spacer
              , label genre' `styleBasic` [textSize 14]
              , spacer
              , longLabel synopsis' `styleBasic` [height 200, textSize 12]
              , spacer
              , longLabel actors' `styleBasic` [height 150, textSize 12]
              ]
          ]
        , [ filler | hasPoster ]
        , [ movieImage (m ^. poster) `styleBasic` [width 350] | hasPoster ]
        ]
