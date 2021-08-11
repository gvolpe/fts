{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Monad.Managed          ( with )
import           Data.String                    ( fromString )
import           Domain.Movie                   ( Movie
                                                , ResultText
                                                , SearchText
                                                )
import           Effects.Display
import           Resources
import           Services.Movies                ( Movies(..)
                                                , mkMovies
                                                )

main :: IO ()
main = with mkResources $ \Res {..} ->
  let Movies {..} = mkMovies postgres
  in  forever $ do
        display ("🔍 Search title: " :: SearchText)
        input <- fromString <$> getLine
        when (input /= "") (program $ findTitle input)
 where
  program :: IO [Movie] -> IO ()
  program runQuery = do
    display ("🎬 Movies" :: ResultText)
    runQuery >>= display
