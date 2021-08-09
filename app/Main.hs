{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.Managed          ( with )
import           Resources
import           Services.Brands                ( mkBrands )
import           Services.Items                 ( mkItems )

main :: IO ()
main = with mkResources $ \Res {..} ->
  let brands = mkBrands postgres
      items  = mkItems postgres
  in  putStrLn "Hello FTS"
