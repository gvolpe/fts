{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Movie where

import           Data.List                      ( intercalate )
import           Data.List.Extra                ( trim )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )
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

newtype MovieYear = MovieYear Int
  deriving stock (Generic, Show)
  deriving FromField via Int

newtype MovieLang = MovieLang Text
  deriving stock (Generic, Show)
  deriving FromField via Text

newtype MovieActors = MovieActors [Text]
  deriving stock (Generic, Show)
  deriving FromField via PGArray Text

newtype MovieDescription = MovieDescription Text
  deriving stock (Generic, Show)
  deriving FromField via Text

newtype SearchText = SearchText Text
  deriving stock (Generic, Show)

instance IsString SearchText where
  fromString = SearchText . T.pack

newtype ResultText = ResultText Text
  deriving stock (Generic, Show)

instance IsString ResultText where
  fromString = ResultText . T.pack

newtype TitleText = TitleText Text
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass ToRow
  deriving ToField via Text

instance IsString TitleText where
  fromString s = TitleText . T.pack $ intercalate " & " (words . trim $ s)

data Movie = Movie
  { movieId          :: MovieId
  , movieName        :: MovieName
  , movieGenre       :: Maybe MovieGenre
  , movieYear        :: Maybe MovieYear
  , movieLang        :: Maybe MovieLang
  , movieDescription :: Maybe MovieDescription
  , movieActors      :: Maybe MovieActors
  }
  deriving (FromRow, Generic, Show)
