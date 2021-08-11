{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Monad.Managed          ( with )
import           Data.Foldable                  ( traverse_ )
import           Domain.Movie                   ( TitleText )
import           Effects.Display
import           Resources
import           Services.Movies                ( Movies(..)
                                                , mkMovies
                                                )

main :: IO ()
main = with mkResources $ \Res {..} ->
  let Movies {..} = mkMovies postgres
  in  do
        display ("Movie Titles" :: TitleText)
        findTitle "matrix" >>= \case
          [] -> putStrLn "> No hits"
          xs -> traverse_ display $ zip ([1..] :: [Int]) xs
