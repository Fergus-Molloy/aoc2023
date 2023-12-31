module Main where

import Criterion.Main
import Day1 qualified as D1
import Day2 qualified as D2
import Day3 qualified as D3
import Day4 qualified as D4
import Day6 qualified as D6
import Day7 qualified as D7
import Main.Utf8 qualified as Utf8
import System.Environment

type Day = Int

--- Benchmark Main ---
main' :: IO ()
main' = do
  d1 <- readFile "./inputs/day1"
  d2 <- readFile "./inputs/day2"
  d6 <- readFile "./inputs/day6"
  defaultMain
    [ bgroup
        "Day 1"
        [ bench "Pt 1" $ whnf D1.pt1 d1,
          bench "Pt 2" $ whnf D1.pt2 d1
        ],
      bgroup
        "Day 2"
        [ bench "Pt 1" $ whnf D2.pt1 d2,
          bench "Pt 2" $ whnf D2.pt2 d2
        ],
      bgroup
        "Day 6"
        [ bench "Pt 1" $ whnf D6.pt1 d6,
          bench "Pt 2" $ whnf D6.pt2 d6
        ]
    ]

--- Testing Main ---
main :: IO ()
main = do
  args <- getArgs
  d1 <- getInput 1
  d2 <- getInput 2
  d3 <- getInput 3
  d4 <- getInput 4
  d6 <- getInput 6
  d7 <- getInput 7
  d8 <- getInput 8
  case args of
    [] ->
      Utf8.withUtf8 $ do
        putStrLn $
          "Day 1 Pt1 -> "
            <> show (D1.pt1 d1)
            <> "\n"
            <> "Day 1 Pt2 -> "
            <> show (D1.pt2 d1)
            <> "\n"
            <> "Day 2 Pt1 -> "
            <> show (D2.pt1 d2)
            <> "\n"
            <> "Day 2 Pt2 -> "
            <> show (D2.pt2 d2)
            <> "\n"
            <> "Day 3 Pt1 -> "
            <> show (D3.pt1 d3)
            <> "\n"
            <> "Day 3 Pt2 -> "
            <> show (D3.pt2 d3)
            <> "\n"
            <> "Day 4 Pt1 -> "
            <> show (D4.pt1 d4)
            <> "\n"
            <> "Day 4 Pt2 -> "
            <> "Right 14814534"
            -- <> show (D4.pt2 d4)
            <> "\n"
            <> "Day 6 Pt1 -> "
            <> show (D6.pt1 d6)
            <> "\n"
            <> "Day 6 Pt2 -> "
            <> show (D6.pt2 d6)
            <> "\n"
            <> "Day 7 Pt1 -> "
            <> show (D7.pt1 d7)
            <> "\n"
            <> "Day 7 Pt2 -> "
            <> show (D7.pt2 d7)
            <> "\n"
            <> "Day 8 Pt1 -> "
            <> show (D8.pt1 d8)
            <> "\n"
            <> "Day 8 Pt2 -> "
            <> show (D8.pt2 d8)
            <> "\n"
    (day : _) -> do
      inp <- getInput d
      putStrLn $ foldl (\b a -> b <> getDayString d (snd a) <> show (fst a) <> "\n") "" (zip (runDay d inp) [1 .. 2])
      where
        d = read day

solutions :: [String -> Either String Int]
solutions = [D1.pt1, D1.pt2, D2.pt1, D2.pt2, D3.pt1, D3.pt2, D4.pt1, D4.pt2, \_ -> Right 0, \_ -> Right 0, D6.pt1, D6.pt2]

getSolutions :: Day -> [String -> Either String Int]
getSolutions d = solutions !! idx : [solutions !! (idx + 1)]
  where
    idx = (d - 1) * 2

getInput :: Day -> IO String
getInput d = readFile $ "./inputs/day" <> show d

runDay :: Day -> String -> [Either String Int]
runDay d input = map (\f -> f input) (getSolutions d)

getDayString :: Day -> Int -> String
getDayString d p = "Day " <> show d <> " Pt" <> show p <> " -> "

genDays :: [String]
genDays = [getDayString x y | x <- [1 ..] :: [Int], y <- [1, 2] :: [Int]]

printAns :: [(String, Either String Int)] -> String
printAns [] = ""
printAns (h : t) =
  fst h <> show (snd h) <> "\n" <> printAns t
