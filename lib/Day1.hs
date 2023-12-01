module Day1 (pt1, pt2, parseAllNumbers, parseNumber) where

import AocParse
import Control.Applicative
import Data.Char
import Data.Functor

parseFirstDigit :: Parser Char
parseFirstDigit = matchWhile (not . isDigit) *> matchDigit

parseFirstAndLast :: Parser Int
parseFirstAndLast = Parser f
  where
    f inp = do
      (_, h) <- runParser parseFirstDigit inp
      (_, t) <- runParser parseFirstDigit (reverse inp)
      Right ("", read [h, t])

parse1 :: Parser [Int]
parse1 = Parser f
  where
    f inp = do
      (_, ls) <- runParser (many matchLine) inp
      nums <- mapM (runParser parseFirstAndLast) ls
      Right ("", map snd nums)

matchString' :: String -> Parser String
matchString' m = Parser f
  where
    f inp = do
      (rest, match) <- runParser (matchString m) inp
      Right (last match : rest, match)

matchTextDigit :: Parser Int
matchTextDigit =
  (matchString' "two" $> 2)
    <|> (matchString' "one" $> 1)
    <|> (matchString' "six" $> 6)
    <|> (matchString' "three" $> 3)
    <|> (matchString' "four" $> 4)
    <|> (matchString' "five" $> 5)
    <|> (matchString' "seven" $> 7)
    <|> (matchString' "eight" $> 8)
    <|> (matchString' "nine" $> 9)

parseNumber :: Parser Int
parseNumber = ((\c -> ord c - 48) <$> matchDigit) <|> matchTextDigit

parseAllNumbers :: [Int] -> Parser [Int]
parseAllNumbers ns = Parser f
  where
    f [] = Right ("", reverse ns)
    f inp = case runParser parseNumber inp of
      Left _ -> runParser (parseAllNumbers ns) (tail inp)
      Right (rest, parsed) -> runParser (parseAllNumbers (parsed : ns)) rest

parse2 :: Parser [Int]
parse2 = Parser f
  where
    f inp = do
      (_, ls) <- runParser (many matchLine) inp
      nums <- mapM (runParser (parseAllNumbers [])) ls
      Right ("", map (\n -> head (snd n) * 10 + last (snd n)) nums)

pt1 :: String -> Either String Int
pt1 inp = do
  (_, nums) <- runParser parse1 inp
  Right $ sum nums

-- (_, parsed) <- runParser parse inp
-- Right $ trace ("parsed: " ++ show parsed) (sum parsed)

pt2 :: String -> Either String Int
pt2 inp = do
  (_, parsed) <- runParser parse2 inp
  Right $ sum parsed
