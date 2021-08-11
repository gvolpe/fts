{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Monad.Managed          ( with )
import           Data.Generics.Labels           ( )
import           Data.String                    ( fromString )
import           Domain.Movie                   ( Movie
                                                , ResultText
                                                , SearchText
                                                )
import           Effects.Display
import           Resources
import           Services.Movies                ( mkMovies )

main :: IO ()
main = with managedMovies $ \movies -> forever $ do
  display ("🔍 Search title: " :: SearchText)
  input <- fromString <$> getLine
  when (input /= "") (program $ input & movies ^. #findTitle)
 where
  managedMovies = mkMovies . view #postgres <$> mkResources
  program :: IO [Movie] -> IO ()
  program runQuery = do
    display ("🎬 Movies" :: ResultText)
    runQuery >>= display
