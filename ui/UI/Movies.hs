{-# LANGUAGE OverloadedStrings #-}

module UI.Movies where

import           Control.Lens
import           Data.Generics.Labels           ( )
import           Data.Maybe
import           Monomer
import qualified Monomer.Lens                  as L
import           TextShow
import           Types.Movie
import           UI.Widgets.MovieDetail         ( movieDetail )
import           UI.Widgets.MovieRow            ( movieRow )

moviesUI
  :: WidgetEnv MoviesModel MoviesEvt
  -> MoviesModel
  -> WidgetNode MoviesModel MoviesEvt
moviesUI wenv model = widgetTree where
  sectionBgColor = wenv ^. L.theme . L.sectionColor

  errorOverlay   = alertMsg msg MoviesCloseError
    where msg = fromMaybe "" (model ^. errorMsg)

  movieOverlay = alert MoviesCloseDetails content
    where content = maybe spacer movieDetail (model ^. selected)

  searchOverlay = box content `styleBasic` [bgColor (darkGray & L.a .~ 0.8)]   where
    content = label "Searching" `styleBasic` [textSize 20, textColor black]

  searchForm = keystroke [("Enter", MoviesSearch)] $ vstack
    [ hstack
          [ label "Query:"
          , spacer
          , textField query `nodeKey` "query"
          , spacer
          , mainButton "Search" MoviesSearch
          ]
        `styleBasic` [bgColor sectionBgColor, padding 25]
    ]

  countLabel = label caption `styleBasic` [padding 10]
    where caption = "Movies (" <> showt (length $ model ^. movies) <> ")"

  moviesChanged old new = old ^. movies /= new ^. movies

  widgetTree = zstack
    [ vstack
      [ searchForm
      , countLabel
      , box_ [mergeRequired moviesChanged]
      $         vscroll (vstack (movieRow wenv <$> model ^. movies))
      `nodeKey` "mainScroll"
      ]
    , errorOverlay `nodeVisible` isJust (model ^. errorMsg)
    , movieOverlay `nodeVisible` isJust (model ^. selected)
    , searchOverlay `nodeVisible` model ^. searching
    ]
