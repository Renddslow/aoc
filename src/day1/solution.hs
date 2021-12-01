module Main where
import System.IO
import Control.Monad
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)

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

partOne :: [Int] -> Int
partOne list = length (findIncrease list)

-- partTwo runs partOne after summing

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

partTwo :: [Int] -> Int
partTwo list = partOne (map sum (windows 3 list))

main = do 
    let list = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singleWords = words contents
        list = f singleWords
    print ("Part 1: " ++ show (partOne list))
    print ("Part 2: " ++ show (partTwo list))
    hClose handle