module Main where

import Criterion.Main
import Day1 qualified as D1
import Day2 qualified as D2
import Day3 qualified as D3
import Main.Utf8 qualified as Utf8
import System.Environment

type Day = Int

--- Benchmark Main ---
main1 :: IO ()
main1 = do
  d1 <- readFile "./inputs/day1"
  d2 <- readFile "./inputs/day2"
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
        ]
    ]

--- Testing Main ---
main :: IO ()
main = do
  args <- getArgs
  d1 <- getInput 1
  d2 <- getInput 2
  d3 <- getInput 3
  case args of
    [] -> Utf8.withUtf8 $ do
      putStrLn $ printAns (zip genDays [D1.pt1 d1, D1.pt2 d1, D2.pt1 d2, D2.pt2 d2, D3.pt1 d3, D3.pt2 d3])
    (day : _) -> do
      inp <- getInput d
      putStrLn $ foldl (\b a -> b <> getDayString d (snd a) <> show (fst a) <> "\n") "" (zip (runDay d inp) [1 .. 2])
      where
        d = read day

solutions :: [String -> Either String Int]
solutions = [D1.pt1, D1.pt2, D2.pt1, D2.pt2, D3.pt1, D3.pt2]

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
