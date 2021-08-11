{-# LANGUAGE FlexibleInstances #-}

module Effects.Display where

import qualified Data.Text                     as T
import           Data.Word                      ( Word8 )
import           Domain.Movie
import           System.Console.ANSI

class Display a where
  display :: a -> IO ()

instance Display (Int, Movie) where
  display (idx, (Movie (MovieId _id) (MovieName _name) _ _ _)) = do
    setSGR
      [ SetConsoleIntensity NormalIntensity
      , SetPaletteColor Foreground lightPurple
      ]
    putStrLn $ show idx <> ". " <> T.unpack _name
    setSGR [Reset]
    putStrLn $ "   ➢ " <> "https://www.imdb.com/title/" <> T.unpack _id

instance Display TitleText where
  display (TitleText t) = do
    setSGR
      [ SetConsoleIntensity BoldIntensity
      , SetColor Foreground Vivid White
      , SetPaletteColor Background steelBlue
      ]
    fullLineText $ T.unpack t
    setSGR [Reset]

instance Display String where
  display = putStrLn

steelBlue :: Word8
steelBlue = xterm6LevelRGB 1 2 3

lightPurple :: Word8
lightPurple = xterm6LevelRGB 4 3 5

fullLineText :: String -> IO ()
fullLineText str = do
  Just (_, y) <- getTerminalSize
  let len = length str
  let lns = (y - (len + 4)) `div` 2
  putStr $ replicate lns '-'
  putStr $ "  " <> str <> "  "
  putStr $ replicate lns '-'

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
