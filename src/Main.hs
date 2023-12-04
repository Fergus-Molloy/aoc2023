module Main where

import Day1 qualified as D1
import Main.Utf8 qualified as Utf8

main :: IO ()
main = do
  d1 <- readFile "./inputs/day1"
  Utf8.withUtf8 $ do
    putStrLn $ printAns (zip genDays [D1.pt1 d1, D1.pt2 d1])

genDays :: [String]
genDays = ["Day " <> show x <> " Pt" <> show y <> " -> " | x <- [1 ..] :: [Int], y <- [1, 2] :: [Int]]

printAns :: [(String, Either String Int)] -> String
printAns [] = ""
printAns (h : t) =
  fst h <> show (snd h) <> "\n" <> printAns t
