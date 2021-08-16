{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Services.Movies
  ( Movies(..)
  , mkMovies
  ) where

import           Control.Exception              ( handle )
import           Database.PostgreSQL.Resilient  ( ResilientConnection(..) )
import           Database.PostgreSQL.Simple
import           Domain.Movie
import           Effects.Display
import           GHC.Generics                   ( Generic )
import           Text.RawString.QQ

newtype Movies m = Movies
  { findTitle :: TitleText -> m [Movie]
  } deriving Generic

mkMovies :: ResilientConnection IO -> Movies IO
mkMovies p = Movies { findTitle = findTitle' p }

findTitle' :: ResilientConnection IO -> TitleText -> IO [Movie]
findTitle' pool txt = do
  conn <- getConnection pool
  withErrorHandler $ query conn byTitleQuery [txt, txt]

withErrorHandler :: IO [a] -> IO [a]
withErrorHandler = handle (\(e :: SqlError) -> [] <$ display e)

byTitleQuery :: Query
byTitleQuery = [r|SELECT title_id, title, genre, year, language, description, actors
     FROM movies
     WHERE ts @@ to_tsquery('english', ?)
     ORDER BY ts_rank(ts, to_tsquery('english', ?)) DESC|]
