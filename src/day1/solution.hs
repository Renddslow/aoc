module Main where
import System.IO
import Control.Monad

mapIdx :: (a -> Int -> b) -> [a] -> [b]
mapIdx f l = zipWith f l [0..]

prevHasIncrease :: [Int] -> Int -> Int -> Bool 
prevHasIncrease list item 0 = False
prevHasIncrease list item idx = (list !! (idx - 1)) < item

isTrue :: Bool -> Bool
isTrue x = x

findIncrease :: [Int] -> [Bool]
findIncrease list = filter isTrue (mapIdx (prevHasIncrease list) list)

f :: [String] -> [Int]
f = map read

main = do 
    let list = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singleWords = words contents
        list = f singleWords
    print (length (findIncrease list))
    hClose handle