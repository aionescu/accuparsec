{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda" #-}

module Main(main) where

import Data.Containers.ListUtils(nubOrd)
import Data.List(sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Random(randomRIO)

import Data.Accuparsec.Text(ParseError(..), ErrorList)
import Data.SList(toList)
import GCL.Parser.Accuparsec qualified as Accu
import GCL.Parser.Attoparsec qualified as Atto

randomInt :: Int -> IO Int
randomInt n = randomRIO (0, n - 1)

location :: Int -> ErrorList -> [Int]
location n =
  take 5 . map (n-) . sort . nubOrd . fmap ((+ 1) . T.length . remainingInput) . toList

closest :: Int -> [Int] -> Int
closest n = minimum . fmap (\x -> abs (x - n))

brokenFile :: String ->  IO (T.Text, Int, Int)
brokenFile name =  do
    file <- T.readFile name
    -- print "Input length"
    -- print $ T.length file
    k <- randomInt $ T.length file
    -- let k = 114
    let (a, b) = T.splitAt k file
    let fixed = T.concat [a, T.tail b]
    return (fixed, k, T.length fixed)

tryAccu :: String -> Int -> IO Int
tryAccu _ 0 = return 0
tryAccu name n = do
  (broken, b, l) <- brokenFile name
  case Accu.parse broken of
      (Left x) -> do
        let z = closest b (location l x)
        -- putStrLn "Accu reported"
        -- print $ location l x
        -- putStrLn "Accu original"
        -- print  b

        r <- tryAccu name (n - 1)
        return (r + z)
      (Right _) -> tryAccu name n

tryAttoParsec :: String -> Int -> IO Int
tryAttoParsec _ 0 = return 0
tryAttoParsec name n = do
  (broken, b, l) <- brokenFile name
  case Atto.parseWithRemainingInput broken of
      (Just x) -> do
        let dist = abs (l - (x + 1) - b)
        -- putStrLn "Atto reported"
        -- print $  l - (x + 1)
        -- putStrLn "Atto original"
        -- print  b
        r <- tryAttoParsec name (n - 1)
        return (r + dist)
      Nothing -> tryAttoParsec name n

errBench :: FilePath -> IO ()
errBench name = do
  print name
  file <- T.readFile name
  putStrLn "Input length"
  print $ T.length file
  case Accu.parse file of
    Left _ -> putStrLn "incorrect input"
    Right _ -> return ()
  s <- tryAccu name 10000
  print s
  s <- tryAttoParsec name 10000
  print s

main :: IO ()
main = do
  mapM_ errBench [
    "progs/err-eval.gcl",
    "progs/err-eval-medium.gcl",
    "progs/err-eval-longest.gcl"]
