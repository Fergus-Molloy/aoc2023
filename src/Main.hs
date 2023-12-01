module Main where

import Day1 qualified as D1
import Main.Utf8 qualified as Utf8

main :: IO ()
main = do
  d1 <- readFile "./inputs/day1"
  Utf8.withUtf8 $ do
    putStrLn $ "Day1 Pt1: " <> show (D1.pt1 d1)
    putStrLn $ "Day1 Pt2: " <> show (D1.pt2 d1)
