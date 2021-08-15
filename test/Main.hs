module Main where

import           Control.Monad                  ( unless )
import           Domain.TitleTextSpec
import           Hedgehog
import           System.Exit

main :: IO ()
main = do
  results <- sequence [checkParallel titleTextSpec]
  unless (and results) exitFailure
