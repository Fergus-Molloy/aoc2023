module Main where

import Day1 qualified as D1
import Day2 qualified as D2
import Main.Utf8 qualified as Utf8
import System.Environment

type Day = Int

main :: IO ()
main = do
  args <- getArgs
  d1 <- getInput 1
  case args of
    [] -> Utf8.withUtf8 $ do
      putStrLn $ printAns (zip genDays [D1.pt1 d1, D1.pt2 d1])
    (day : _) -> do
      inp <- getInput d
      putStrLn $ foldl (\b a -> b <> getDayString d (snd a) <> show (fst a) <> "\n") "" (zip (runDay d inp) [1 .. 2])
      where
        d = read day

solutions :: [String -> Either String Int]
solutions = [D1.pt1, D1.pt2, D2.pt1, D2.pt2]

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
