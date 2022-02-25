module Main where

import System.IO

import Control.Applicative

depthFilter :: String -> Bool
depthFilter a = do
    let coords = words a
    head coords == "down" || head coords == "up"

forwardFilter :: String -> Bool
forwardFilter a = do
    let coords = words a
    head coords == "forward"

convertDepthValue :: String -> Int
convertDepthValue a = do
    let coords = words a
    let num = read (coords !! (length coords - 1))
    if head coords == "up" then negate num else num

convertForwardValue :: String -> Int
convertForwardValue a = do
    let coords = words a
    read (coords !! (length coords - 1))

findFinalDepth :: [String] -> Int
findFinalDepth list = sum (map convertDepthValue (filter depthFilter list))

findFinalForward :: [String] -> Int
findFinalForward list = sum (map convertForwardValue (filter forwardFilter list))

partOne :: [String] -> Int
partOne list = findFinalDepth list * findFinalForward list

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let list = lines contents
    print ("Part 1: " ++ show (partOne list))
    hClose handle