{-# LANGUAGE OverloadedLabels, OverloadedStrings, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Managed          ( with )
import           Data.Default
import           Data.Either.Extra
import           Data.Generics.Labels           ( )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Domain.Movie
import           TextShow

import qualified Data.Text                     as T
import qualified Network.Wreq                  as W

import           Monomer
import           MovieTypes
import           Resources
import           Services.Movies                ( Movies
                                                , mkMovies
                                                )


import qualified Monomer.Lens                  as L

type MoviesWenv = WidgetEnv MoviesModel MoviesEvt
type MoviesNode = WidgetNode MoviesModel MoviesEvt

main :: IO ()
main = with managedMovies $ \service -> do
  startApp initModel (eventHandler service) moviesUI config
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

movieImage :: Maybe Text -> WidgetNode s MoviesEvt
movieImage imgId = maybe filler coverImg imgId where
  baseUrl = "https://www.themoviedb.org/t/p/w1280"
  coverImg i = image_ (baseUrl <> i) [fitHeight, alignRight]

fetchPoster :: Movie -> IO (Maybe String)
fetchPoster (Movie (MovieId tid) _ _ _ _) = do
  let url = posterUrl tid
  putStrLn $ "Querying: " <> url
  m <- fetch url
  return $ poster_path . head . movie_results <$> m
 where
  fetch :: String -> IO (Maybe MovieResp)
  fetch url =
    W.get url >>= W.asJSON >>= return . preview (W.responseBody . _Just)

posterUrl :: Text -> String
posterUrl tid = T.unpack $ T.replace "<api_key>" apiKey $ T.replace
  "<title_id>"
  tid
  baseUrl
 where
  baseUrl
    = "https://api.themoviedb.org/3/find/<title_id>?api_key=<api_key>&language=en-US&external_source=imdb_id"
  apiKey = ""

searchMovies :: Movies IO -> Text -> IO MoviesEvt
searchMovies service input = do
  m   <- toTitleText input & service ^. #findTitle
  res <- traverse (\x -> (`fromMovie` x) <$> fetchPoster x) m
  return $ MoviesSearchResult res

eventHandler
  :: Movies IO
  -> WidgetEnv MoviesModel MoviesEvt
  -> WidgetNode MoviesModel MoviesEvt
  -> MoviesModel
  -> MoviesEvt
  -> [EventResponse MoviesModel MoviesEvt MoviesModel ()]
eventHandler service wenv _ model evt = case evt of
  MoviesInit -> [setFocusOnKey wenv "query"]
  MoviesSearch ->
    [ Model $ model & searching .~ True
    , Task $ searchMovies service (model ^. query)
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

movieRow :: MoviesWenv -> MovieDTO -> MoviesNode
movieRow wenv m = row where
  rowBgColor  = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  publishYear = "2021" -- maybe "" showt (m ^. year)

  rowContent m = hstack
    [ vstack
      [ label_ (m ^. titleName) [resizeFactor 1]
        `styleBasic` [textFont "Medium", textSize 16]
      , spacer
      , label_ (m ^. titleId) [resizeFactor 1] `styleBasic` [textSize 14]
      ]
    , filler
    , vstack [label publishYear `styleBasic` [width 50, textSize 14], spacer]
    , movieImage (m ^. poster) `styleBasic` [width 35]
    ]

  row = box_ cfg content `styleBasic` [padding 10, paddingT 0]   where
    cfg = [expandContent, onClick (MoviesShowDetails m)]
    content =
      rowContent m
        `styleBasic` [height 80, padding 20, radius 5]
        `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

movieDetail :: MovieDTO -> WidgetNode s MoviesEvt
movieDetail m = content `styleBasic` [minWidth 600, paddingH 20] where
  hasPoster   = isJust (m ^. poster)
  publishYear = "2021" -- maybe "" showt (m ^. year)

  --shortLabel value = label value `styleBasic` [textFont "Medium", textTop]
  longLabel value = label_ value [multiline, ellipsis, trimSpaces]

  content =
    hstack
      . concat
      $ [ [ vstack
              [ longLabel (m ^. titleName)
                `styleBasic` [textSize 20, textFont "Medium"]
              , spacer
              , longLabel (m ^. titleId) `styleBasic` [textSize 16]
              , spacer
              , label publishYear `styleBasic` [textSize 14]
              ]
          ]
        , [filler]
        , [ movieImage (m ^. poster) `styleBasic` [width 350] | hasPoster ]
        ]

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

customLightTheme :: Theme
customLightTheme =
  lightTheme & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme =
  darkTheme & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

-- Utility function to avoid the "Ambiguous type variable..." error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch
