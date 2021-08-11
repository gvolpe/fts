{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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

newtype TitleText = TitleText Text deriving (Generic, Read, Show, ToRow)

instance IsString TitleText where
  fromString s =
    let ys = zip [0 ..] $ words s
        f  = \(i, w) -> if even i then w else "& " <> w
    in  TitleText $ T.pack (unwords $ f <$> ys)

data Movie = Movie
  { movieId :: MovieId
  , movieName :: MovieName
  , movieGenre :: Maybe MovieGenre
  , movieCountry :: Maybe MovieCountry
  , movieLang :: Maybe MovieLang
  } deriving (FromRow, Generic, Show)
