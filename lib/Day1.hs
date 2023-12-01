module Day1 (pt1, pt2, parseFirstAndLast) where

import AocParse
import Control.Applicative
import Data.Char

parseNumber :: Parser Char
parseNumber = matchWhile (not . isDigit) *> matchDigit

parseFirstAndLast :: Parser Int
parseFirstAndLast = Parser f
  where
    f inp = do
      (_, h) <- runParser parseNumber inp
      (_, t) <- runParser parseNumber (reverse inp)
      Right ("", read [h, t])

parse :: Parser [Int]
parse = Parser f
  where
    f inp = do
      (_, ls) <- runParser (many matchLine) inp
      nums <- mapM (runParser parseFirstAndLast) ls
      Right ("", map snd nums)

pt1 :: String -> Either String Int
pt1 inp = do
  (_, nums) <- runParser parse inp
  Right $ sum nums

-- (_, parsed) <- runParser parse inp
-- Right $ trace ("parsed: " ++ show parsed) (sum parsed)

pt2 :: String -> Int
pt2 _ = 0
