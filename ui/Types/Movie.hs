{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Movie where

import           Control.Lens.TH
import           Data.Aeson
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
  , _mvActors    :: [String]
  }
  deriving (Eq, Show)

toTitleText :: Text -> TitleText
toTitleText = fromString . T.unpack

fromMovie :: Maybe String -> Movie -> MovieDTO
fromMovie poster (Movie (MovieId _id) (MovieName _name) maybeGender maybeYear _ maybeDesc maybeActors)
  = MovieDTO _id _name _year _desc _genre (T.pack <$> poster) _actors
 where
  _desc   = (\(MovieDescription y) -> y) <$> maybeDesc
  _year   = (\(MovieYear y) -> y) <$> maybeYear
  _genre  = (\(MovieGenre y) -> y) <$> maybeGender
  _actors = maybe [] (\(MovieActors y) -> y) maybeActors

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
