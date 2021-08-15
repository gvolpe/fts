{-# LANGUAGE OverloadedStrings #-}

module HTTP.Posters
  ( fetchPoster
  , ApiKey
  ) where

import           Control.Lens
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

fetchPoster :: Maybe ApiKey -> Movie -> IO (Maybe Poster)
fetchPoster Nothing _ =
  Nothing <$ putStrLn "No value set for TMDB_API_KEY, cannot get movie poster"
fetchPoster (Just apiKey) (Movie (MovieId tid) _ _ _ _ _) = do
  let url = posterUrl apiKey tid
  putStrLn $ "Fetching poster: " <> redact url
  m <- fetch url `orElse` return Nothing
  return $ poster_path <$> (m >>= listToMaybe . movie_results)
 where
  fetch :: String -> IO (Maybe MovieResp)
  fetch url = (W.get url >>= W.asJSON) <&> preview (W.responseBody . _Just)

posterUrl :: ApiKey -> Text -> String
posterUrl apiKey tid =
  T.unpack (T.replace "<api_key>" apiKey $ T.replace "<title_id>" tid baseUrl)
    <> params
 where
  baseUrl = "https://api.themoviedb.org/3/find/<title_id>?api_key=<api_key>"
  params  = "&language=en-US&external_source=imdb_id"

redact :: String -> String
redact url = take 52 url <> "<REDACTED>"
