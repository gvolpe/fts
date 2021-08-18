{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP.Posters
  ( fetchPoster
  , ApiKey
  ) where

import           Control.Lens
import           Data.Coerce                    ( coerce )
import           Data.Generics.Labels           ( )
import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Domain.Movie
import qualified Network.Wreq                  as W
import           Types.Movie
import           Utils                          ( orElse )

type ApiKey = Text
type Poster = String

fetchPoster :: ApiKey -> Movie -> IO (Maybe Poster)
fetchPoster apiKey mv = do
  let url = posterUrl apiKey $ coerce (mv ^. #movieId)
  putStrLn $ "Fetching poster: " <> redact url
  m <- fetch url `orElse` return Nothing
  return $ poster_path <$> (m >>= listToMaybe . movie_results)
 where
  fetch :: String -> IO (Maybe MovieResp)
  fetch url = (W.get url >>= W.asJSON) <&> preview (W.responseBody . _Just)

posterUrl :: ApiKey -> Text -> String
posterUrl key tid =
  let url = "https://api.themoviedb.org/3/find/<title_id>?api_key=<api_key>"
      ext = "&language=en-US&external_source=imdb_id"
  in  T.unpack (T.replace "<api_key>" key $ T.replace "<title_id>" tid url) <> ext

redact :: String -> String
redact url = takeWhile (/= '=') url <> "=<REDACTED>"
