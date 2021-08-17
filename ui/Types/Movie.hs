{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Movie where

import           Control.Lens                   ( (^.) )
import           Control.Lens.TH
import           Data.Aeson
import           Data.Coerce                    ( coerce )
import           Data.Generics.Labels           ( )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Domain.Movie
import           GHC.Generics                   ( Generic )
import           Monomer                        ( WidgetEnv(..)
                                                , WidgetNode(..)
                                                )

type MoviesWenv = WidgetEnv MoviesModel MoviesEvt
type MoviesNode = WidgetNode MoviesModel MoviesEvt

data MovieDTO = MovieDTO
  { _mvTitleId   :: Text
  , _mvTitleName :: Text
  , _mvYear      :: Maybe Int
  , _mvSynopsis  :: Maybe Text
  , _mvGenre     :: Maybe Text
  , _mvPoster    :: Maybe Text
  , _mvActors    :: [Text]
  }
  deriving (Eq, Show)

toTitleText :: Text -> TitleText
toTitleText = fromString . T.unpack

fromMovie :: Maybe String -> Movie -> MovieDTO
fromMovie poster m = MovieDTO { _mvTitleId   = coerce $ m ^. #movieId
                              , _mvTitleName = coerce $ m ^. #movieName
                              , _mvYear      = coerce <$> m ^. #movieYear
                              , _mvSynopsis  = coerce <$> m ^. #movieDescription
                              , _mvGenre     = coerce <$> m ^. #movieGenre
                              , _mvPoster    = T.pack <$> poster
                              , _mvActors = maybe [] coerce $ m ^. #movieActors
                              }

data MoviesModel = MoviesModel
  { _mvmQuery     :: Text
  , _mvmSearching :: Bool
  , _mvmErrorMsg  :: Maybe Text
  , _mvmMovies    :: [MovieDTO]
  , _mvmSelected  :: Maybe MovieDTO
  }
  deriving (Eq, Show)

data MoviesEvt
  = MoviesInit
  | MoviesSearch
  | MoviesSearchResult [MovieDTO]
  | MoviesSearchError Text
  | MoviesShowDetails MovieDTO
  | MoviesCloseDetails
  | MoviesCloseError
  deriving (Eq, Show)

data MovieResult = MovieResult
  { overview    :: String
  , title       :: String
  , poster_path :: String
  }
  deriving (Generic, FromJSON, Show)

newtype MovieResp = MovieResp
  { movie_results :: [MovieResult]
  } deriving (Generic, FromJSON, Show)

makeLensesWith abbreviatedFields 'MovieDTO
makeLensesWith abbreviatedFields 'MoviesModel
