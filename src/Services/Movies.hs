{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Services.Movies
  ( Movies(..)
  , mkMovies
  )
where

import           Database.PostgreSQL.Resilient  ( ResilientConnection(..) )
import           Database.PostgreSQL.Simple
import           Domain.Movie
import           GHC.Generics                   ( Generic )
import           Text.RawString.QQ

data Movies m = Movies
  { findTitle :: TitleText -> m [Movie]
  } deriving Generic

mkMovies :: ResilientConnection IO -> Movies IO
mkMovies p =  Movies { findTitle = findTitle' p }

findTitle' :: ResilientConnection IO -> TitleText -> IO [Movie]
findTitle' pool txt = do
  conn <- getConnection pool
  query conn byTitleQuery [txt, txt]

byTitleQuery :: Query
byTitleQuery =
  [r|SELECT title_id, title, genre, country, language
     FROM movies
     WHERE ts @@ to_tsquery('english', ?)
     ORDER BY ts_rank(ts, to_tsquery('english', ?)) DESC|]
