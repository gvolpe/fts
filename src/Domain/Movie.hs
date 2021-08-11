{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingVia, OverloadedStrings #-}

module Domain.Movie
  ( MovieId(..)
  , MovieName(..)
  , Movie(..)
  , TitleText(..)
  )
where

import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           GHC.Generics                   ( Generic )

newtype MovieId = MovieId Text
  deriving stock (Generic, Show)
  deriving FromField via Text

newtype MovieName = MovieName Text
  deriving stock (Generic, Show)
  deriving FromField via Text

newtype MovieGenre = MovieGenre Text
  deriving stock (Generic, Show)
  deriving FromField via Text

newtype MovieCountry = MovieCountry Text
  deriving stock (Generic, Show)
  deriving FromField via Text

newtype MovieLang = MovieLang Text
  deriving stock (Generic, Show)
  deriving FromField via Text

newtype TitleText = TitleText Text deriving (Generic, ToRow, Show)

instance IsString TitleText where
  fromString = TitleText . T.pack

data Movie = Movie
  { movieId :: MovieId
  , movieName :: MovieName
  , movieGenre :: MovieGenre
  , movieCountry :: MovieCountry
  , movieLang :: MovieLang
  } deriving (FromRow, Generic)

instance Show Movie where
  show (Movie (MovieId _id) (MovieName _name) (MovieGenre _genre) (MovieCountry _country) (MovieLang _lang))
    = "> " <> T.unpack _name
