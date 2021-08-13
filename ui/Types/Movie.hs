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
  , _mvPoster    :: Maybe Text
  }
  deriving (Eq, Show)

toTitleText :: Text -> TitleText
toTitleText = fromString . T.unpack

fromMovie :: Maybe String -> Movie -> MovieDTO
fromMovie poster (Movie (MovieId _id) (MovieName _name) _ maybeYear _ maybeDesc)
  = MovieDTO _id _name _year _desc (T.pack <$> poster)
 where
  _desc = (\(MovieDescription y) -> y) <$> maybeDesc
  _year = (\(MovieYear y) -> y) <$> maybeYear

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
