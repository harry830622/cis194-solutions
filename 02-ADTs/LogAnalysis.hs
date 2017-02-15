{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage line
  | messageType == 'I' = LogMessage Info
                         ((read $ tokens !! 1) :: Int)
                         ((unwords . drop 2) tokens)
  | messageType == 'W' = LogMessage Warning
                         ((read $ tokens !! 1) :: Int)
                         ((unwords . drop 2) tokens)
  | messageType == 'E' = LogMessage (Error ((read $ tokens !! 1) :: Int))
                         ((read $ tokens !! 2) :: Int)
                         ((unwords . drop 3) tokens)
  | otherwise = Unknown line
  where messageType = head line
        tokens = words line

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage (Leaf) = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ timestamp _)
       (Node leftChild
             currentLogMessage@(LogMessage _ currentTimestamp _)
             rightChild)
  | timestamp < currentTimestamp = Node (insert logMessage leftChild)
                                        currentLogMessage
                                        rightChild
  | otherwise = Node leftChild currentLogMessage (insert logMessage rightChild)
insert _ messageTree = messageTree

-- Exercise 3
build :: [LogMessage] -> MessageTree
-- build = foldl (flip insert) Leaf
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node leftChild logMessage rightChild) = inOrder leftChild ++
                                                 logMessage : inOrder rightChild

-- Exercise 5
isSevere :: LogMessage -> Bool
isSevere (Unknown _) = False
isSevere (LogMessage (Error severity) _ _)
  | severity >= 50 = True
  | otherwise = False
isSevere _ = False

extractString :: LogMessage -> String
extractString (LogMessage _ _ string) = string
extractString _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractString . filter isSevere . inOrder . build
