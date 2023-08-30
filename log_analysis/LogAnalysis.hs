{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage' :: [String] -> LogMessage
parseMessage' ("E":sev:ts:msg) = LogMessage (Error $ read sev) (read ts) (unwords msg)
parseMessage' ("W":ts:msg)     = LogMessage Warning (read ts) (unwords msg)
parseMessage' ("I":ts:msg)     = LogMessage Info (read ts) (unwords msg)
parseMessage' xs               = Unknown    (unwords xs)

parseMessage :: String -> LogMessage
parseMessage x = parseMessage' $ words x

parse :: String -> [LogMessage]
parse xs = map parseMessage $ lines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert _ (Node _ (Unknown _) _) = error "invalid tree"
insert (Unknown _) _  = error "cant insert unknown message" 
insert lm Leaf = Node Leaf lm Leaf  
insert lm@(LogMessage _ ts0 _) (Node left lm1@(LogMessage _ ts _) right)
    | ts0 < ts = Node (insert lm left) lm1 right
    | otherwise = Node left lm1 (insert lm right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build xs = foldl (flip insert) Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error sev) _ _) = sev > 50
isSevere _ = False

isInfo :: LogMessage -> Bool
isInfo (LogMessage Info _ _) = True
isInfo _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage _ = error "cant get message from unknown"

whatWentWrong :: (LogMessage -> Bool) -> [LogMessage] -> [String]
whatWentWrong f lms = map getMessage $ filter f $ inOrder $ build lms

main :: IO ()
main = 
    do 
        logs <- testParse parse  10 "error.log"
        print $ inOrder $ build logs 

