{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Effects.Display where

import           Control.Lens                   ( (^.) )
import           Data.Coerce                    ( coerce )
import           Data.Foldable                  ( traverse_ )
import           Data.Generics.Labels           ( )
import qualified Data.Text                     as T
import           Data.Word                      ( Word8 )
import           Database.PostgreSQL.Simple     ( SqlError(..) )
import           Domain.Movie
import           System.Console.ANSI
import           System.IO

class Display a where
  display :: a -> IO ()

instance Display [Movie] where
  display [] = putStrLn "➢ No hits"
  display xs = traverse_ f $ zip [1 ..] xs
   where
    f (idx, m) = do
      setSGR
        [ SetConsoleIntensity NormalIntensity
        , SetPaletteColor Foreground paleOrange
        ]
      putStrLn $ show idx <> ". " <> T.unpack (coerce $ m ^. #movieName)
      setSGR [Reset]
      putStrLn $ "   ➢ " <> "https://www.imdb.com/title/" <> T.unpack
        (coerce $ m ^. #movieId)

instance Display ResultText where
  display (ResultText t) = do
    setSGR
      [ SetConsoleIntensity BoldIntensity
      , SetColor Foreground Vivid White
      , SetPaletteColor Background darkRed
      ]
    fullLineText $ T.unpack t
    setSGR [Reset]
    putStrLn ""

instance Display SearchText where
  display (SearchText t) = do
    putStrLn ""
    setSGR
      [ SetConsoleIntensity BoldIntensity
      , SetColor Foreground Vivid White
      , SetPaletteColor Background darkOrange
      ]
    putStr $ T.unpack t
    hFlush stdout
    setSGR [Reset]

instance Display SqlError where
  display (SqlError _ _ msg _ _) = do
    setSGR
      [ SetConsoleIntensity BoldIntensity
      , SetColor Foreground Vivid White
      , SetColor Background Vivid Red
      ]
    putStr $ "SQL Error: " <> show msg
    hFlush stdout
    setSGR [Reset]
    putStrLn ""

instance Display String where
  display = putStrLn

darkPurple :: Word8
darkPurple = xterm6LevelRGB 1 0 5

darkRed :: Word8
darkRed = xterm6LevelRGB 1 0 0

lightPurple :: Word8
lightPurple = xterm6LevelRGB 4 3 5

darkOrange :: Word8
darkOrange = xterm6LevelRGB 4 1 0

paleOrange :: Word8
paleOrange = xterm6LevelRGB 4 3 1

steelBlue :: Word8
steelBlue = xterm6LevelRGB 1 2 3

fullLineText :: String -> IO ()
fullLineText str = do
  Just (_, y) <- getTerminalSize
  let len = length str
  let lns = (y - (len + 4)) `div` 2
  putStr $ replicate lns '—'
  putStr $ " " <> str <> "  "
  putStr $ replicate lns '—'

showRGBCode :: Int -> Int -> Int -> IO ()
showRGBCode r g b =
  let idx = 16 + 36 * r + 6 * g + b
  in  putStrLn
        $  "RGB index code for ("
        <> show r
        <> ","
        <> show g
        <> ","
        <> show b
        <> ") = "
        <> show idx
