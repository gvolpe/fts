{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.MovieDetail where

import           Control.Lens                   ( (^.) )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Text                      ( intercalate )
import qualified Data.Text                     as T
import           Monomer
import           TextShow                       ( showt )
import           Types.Movie
import           UI.Widgets.MovieImage          ( movieImage
                                                , tmdbImage
                                                )

movieDetail :: MovieDTO -> WidgetNode s MoviesEvt
movieDetail m = content `styleBasic` [minWidth 600, paddingH 20] where
  hasPoster   = isJust (m ^. poster)
  synopsis'   = fromMaybe "" (m ^. synopsis)
  publishYear = maybe "Unknown" showt (m ^. year)
  genre'      = fromMaybe "Unknown" (m ^. genre)
  actors'     = T.filter (/= '"') $ intercalate ", " (m ^. actors)

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
              , spacer
              , tmdbImage `styleBasic` [width 250]
              ]
          ]
        , [ filler | hasPoster ]
        , [ movieImage (m ^. poster) `styleBasic` [width 350] | hasPoster ]
        ]
