{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda" #-}

module Main(main) where

import Data.Containers.ListUtils(nubOrd)
import Data.List(sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Random(randomRIO)

import Data.Accuparsec.Text(ErrorList, remainingInput)
import Data.SList(toList)
import GCL.Parser.Accuparsec qualified as Accu
import GCL.Parser.Attoparsec qualified as Atto

randomInt :: Int -> IO Int
randomInt n = randomRIO (0, n - 1)

location :: Int -> ErrorList -> [Int]
location n =
  take 5 . sort . nubOrd . fmap ((n -) . (+ 1) . T.length . remainingInput) . Data.SList.toList

closest :: Int -> [Int] -> Int
closest n = minimum . fmap (\x -> abs (x - n))

brokenFile :: IO (T.Text, Int, Int)
brokenFile =  do
    file <- T.readFile "progs/err-eval.gcl"
    -- r <- randomInt $ T.length file
    let k = 114
    let (a, b) = T.splitAt k file
    let fixed = T.concat [a, T.tail b]
    return (fixed, k, T.length fixed)

tryAccu :: Int -> IO Int
tryAccu 0 = return 0
tryAccu n = do
  (broken, b, l) <- brokenFile
  case Accu.parse broken of
      (Left x) -> do
        let z = closest b (location l x)
        putStrLn "Accu reported"
        print $ location l x
        putStrLn "Accu original"
        print  b

        r <- tryAccu (n - 1)
        return (r + z)
      (Right _) -> tryAccu n

tryAttoParsec :: Int -> IO Int
tryAttoParsec 0 = return 0
tryAttoParsec n = do
  (broken, b, l) <- brokenFile
  case Atto.parseWithRemainingInput broken of
      (Just x) -> do
        let dist = abs (l - (x + 1) - b)
        putStrLn "Atto reported"
        print $  l - (x + 1)
        putStrLn "Atto original"
        print  b
        r <- tryAttoParsec (n - 1)
        return (r + dist)
      Nothing -> tryAttoParsec n

main :: IO ()
main = do
  s <- tryAccu 1
  print s
  s <- tryAttoParsec 1
  print s
