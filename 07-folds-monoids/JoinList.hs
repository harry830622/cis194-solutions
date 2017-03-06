{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Data.Monoid

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Excersise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

-- Excersise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ a) | i == 0 = Just a
                      | otherwise = Nothing
indexJ i (Append _ l r) | i < l_size = indexJ i l
                        | otherwise = indexJ (i - l_size) r
  where l_size = getSize . size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i j | i <= 0 = j
dropJ _ (Single _ _) = Empty
dropJ i (Append _ l r) | i <= l_size = (dropJ i l) +++ r
                       | otherwise = dropJ (i - l_size) r
  where l_size = getSize . size $ tag l
