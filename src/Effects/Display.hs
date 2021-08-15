{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}

module Effects.Display where

import           Data.Foldable                  ( traverse_ )
import qualified Data.Text                     as T
import           Data.Word                      ( Word8 )
import           Domain.Movie
import           System.Console.ANSI
import           System.IO

class Display a where
  display :: a -> IO ()

instance Display [Movie] where
  display [] = putStrLn "➢ No hits"
  display xs = traverse_ f $ zip [1 ..] xs
   where
    f (idx, Movie (MovieId _id) (MovieName _name) _ _ _ _) = do
      setSGR
        [ SetConsoleIntensity NormalIntensity
        , SetPaletteColor Foreground paleOrange
        ]
      putStrLn $ show idx <> ". " <> T.unpack _name
      setSGR [Reset]
      putStrLn $ "   ➢ " <> "https://www.imdb.com/title/" <> T.unpack _id

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
