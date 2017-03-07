{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

-- Exercise 3
import Data.Char

newtype Score = Score Int
  deriving (Eq, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | c_lower `elem` "aeilnorstu" = Score 1
  | c_lower `elem` "dg"         = Score 2
  | c_lower `elem` "bcmp"       = Score 3
  | c_lower `elem` "fhvwy"      = Score 4
  | c_lower `elem` "k"          = Score 5
  | c_lower `elem` "jx"         = Score 8
  | c_lower `elem` "qz"         = Score 10
  | otherwise                   = Score 0
  where c_lower = toLower c

scoreString :: String -> Score
scoreString = foldl (\n c -> n + score c) (Score 0)
