{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Monad.Managed          ( with )
import           Data.Foldable                  ( traverse_ )
import           Resources
import           Services.Movies                ( Movies(..)
                                                , mkMovies
                                                )

main :: IO ()
main = with mkResources $ \Res {..} ->
  let Movies {..} = mkMovies postgres
  in  do
        hits <- findTitle "gamble"
        traverse_ print hits
