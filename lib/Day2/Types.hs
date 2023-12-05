module Day2.Types where

data Cube = Red Int | Green Int | Blue Int deriving stock (Show)

data Handful = Handful
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving stock (Show)

data Game = Game
  { id :: Int,
    handfuls :: [Handful]
  }
  deriving stock (Show)
