module Day4 (pt1, pt2) where

import AocParse
import Data.Bits

data Card = Card {ident :: Int, wins :: [Int], numbers :: [Int]} deriving stock (Show)

parseIntList :: Parser [Int]
parseIntList = splitOn matchWhiteSpace matchInt

parseCard :: Parser Card
parseCard = Parser f
  where
    f inp = do
      (lists, identifier) <- runParser (matchString "Card" *> matchWhiteSpace *> matchInt <* matchString ":" <* matchWhiteSpace) inp
      (rest, winning) <- runParser (parseIntList <* matchWhiteSpace <* matchChar '|' <* matchWhiteSpace) lists
      (rest', nums) <- runParser parseIntList rest
      Right (rest', Card identifier winning nums)

parse :: Parser [Card]
parse = splitOn matchWhiteSpace parseCard

getWinCount :: Card -> Int
getWinCount card = length (filter (\n -> n `elem` wins card) $ numbers card)

getScore :: Card -> Int
getScore card = if (winCount - 1) == -1 then 0 else 1 `shiftL` winCount
  where
    winCount = getWinCount card

getCopies :: [Card] -> Card -> Int
getCopies cs c = 1 + sum (map (getCopies cs) (take winCount $ drop (ident c) cs))
  where
    winCount = getWinCount c

-- getCopies' :: [Card] -> Card -> Int
-- getCopies' (c : cs) c = 1 + sum (map (getCopies cs) (take winCount $ drop (ident c) cs))
--   where
--     winCount = getWinCount c

pt1 :: String -> Either String Int
pt1 inp = do
  (_, parsed) <- runParser parse inp
  Right $ sum $ map getScore parsed

pt2 :: String -> Either String Int
pt2 inp = do
  (_, parsed) <- runParser parse inp
  Right $ sum $ map (getCopies parsed) parsed
