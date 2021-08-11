{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Monad                  ( forever )
import           Control.Monad.Managed          ( with )
import           Data.Foldable                  ( traverse_ )
import           Data.String                    ( fromString )
import           Domain.Movie                   ( TitleText )
import           Effects.Display
import           Resources
import           Services.Movies                ( Movies(..)
                                                , mkMovies
                                                )
import           System.IO

main :: IO ()
main = with mkResources $ \Res {..} ->
  let Movies {..} = mkMovies postgres
  in  forever $ do
        putStr "➢ Search title: "
        hFlush stdout
        input <- fromString <$> getLine
        display ("Movies" :: TitleText)
        putStrLn ""
        findTitle input >>= \case
          [] -> putStrLn "➢ No hits"
          xs -> traverse_ display $ zip [1 ..] xs
        putStrLn ""
