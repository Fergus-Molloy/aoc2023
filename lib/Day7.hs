module Day7 where

import AocParse
import Control.Applicative
import Data.List

data Card
  = Ace
  | King
  | Queen
  | Jack
  | Number Int
  deriving stock (Eq)

data Hand
  = Five [Card]
  | Four [Card]
  | FullHouse [Card]
  | Three [Card]
  | TwoPair [Card]
  | OnePair [Card]
  | HighCard [Card]
  deriving stock (Eq, Show)

isLower :: [Card] -> [Card] -> Bool
isLower (a : as) (b : bs) = if a == b then isLower as bs else a < b
isLower _ _ = False

instance Ord Hand where
  (<=) (Five as) (Five bs) = as `isLower` bs
  (<=) (Four as) (Four bs) = as `isLower` bs
  (<=) (FullHouse as) (FullHouse bs) = as `isLower` bs
  (<=) (Three as) (Three bs) = as `isLower` bs
  (<=) (TwoPair as) (TwoPair bs) = as `isLower` bs
  (<=) (OnePair as) (OnePair bs) = as `isLower` bs
  (<=) (HighCard as) (HighCard bs) = as `isLower` bs
  (<=) (FullHouse _) (Four _) = True
  (<=) (Three _) (Four _) = True
  (<=) (TwoPair _) (Four _) = True
  (<=) (OnePair _) (Four _) = True
  (<=) (HighCard _) (Four _) = True
  (<=) (Three _) (FullHouse _) = True
  (<=) (TwoPair _) (FullHouse _) = True
  (<=) (OnePair _) (FullHouse _) = True
  (<=) (HighCard _) (FullHouse _) = True
  (<=) (TwoPair _) (Three _) = True
  (<=) (OnePair _) (Three _) = True
  (<=) (HighCard _) (Three _) = True
  (<=) (OnePair _) (TwoPair _) = True
  (<=) (HighCard _) (TwoPair _) = True
  (<=) (HighCard _) (OnePair _) = True
  (<=) _ _ = False

instance Show Card where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show (Number 10) = "T"
  show (Number a) = show a

instance Ord Card where
  (<=) a b | a == b = True
  (<=) a b = case (a, b) of
    (_, Ace) -> True
    (Queen, King) -> True
    (Jack, King) -> True
    (Number _, King) -> True
    (Jack, Queen) -> True
    (Number _, Jack) -> True
    (Number _, Queen) -> True
    (Number x, Number y) -> x <= y
    _ -> False

parseChar :: Parser Card
parseChar =
  (Ace <$ matchChar 'A')
    <|> (King <$ matchChar 'K')
    <|> (Queen <$ matchChar 'Q')
    <|> (Jack <$ matchChar 'J')
    <|> (Number 10 <$ matchChar 'T')

parseCards :: Parser [Card]
parseCards = many $ parseChar <|> (Number . (\c -> read [c]) <$> matchDigit)

parseLine :: Parser ([Card], Int)
parseLine = Parser f
  where
    f inp = do
      (rest, cards) <- runParser parseCards inp
      (rest', bid) <- runParser (matchWhiteSpace *> matchInt) rest
      Right (rest', (cards, bid))

parse :: Parser [([Card], Int)]
parse = splitOn matchWhiteSpace parseLine

getHand :: [Card] -> Hand
getHand cs = case maximum groupLengths of
  5 -> Five cs
  4 -> Four cs
  3 | 2 == length groupLengths -> FullHouse cs
  3 -> Three cs
  2 | 2 == length (filter (>= 2) groupLengths) -> TwoPair cs
  2 -> OnePair cs
  _ -> HighCard cs
  where
    groupLengths :: [Int]
    groupLengths = map length groups
    groups = group $ sort cs

tinp :: String
tinp = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

pt1 :: String -> Either String Int
pt1 inp = do
  (_, parsed) <- runParser parse inp
  hands <- Right $ zip (map (getHand . fst) parsed) (map snd parsed)
  handBids <- Right $ zip (sort hands) [1 ..]
  Right (sum $ map (\((_, bid), rank) -> bid * rank) handBids)

pt2 :: String -> Either String Int
pt2 _ = Right 0
