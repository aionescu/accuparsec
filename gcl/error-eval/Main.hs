-- module Main(main) where

-- import Control.DeepSeq
-- import Criterion.Main

-- import Data.Text(Text)
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda" #-}
import Data.Text.IO qualified as T
import GCL.Parser.Accuparsec qualified as Accu
import Data.SList (SList (Nil, (:!)), toList)
import Data.Accuparsec.Text (ParseError (Error, Label), remainingInput)
import qualified Data.Text as Tx
import Data.Set (toList, fromList)
import GHC.IO (unsafePerformIO)
import System.IO.Unsafe
import System.Random
import GCL.Parser.Attoparsec qualified as Atto
import Data.List (sort)



uniquify :: Ord a => [a] -> [a]
uniquify lst = Data.Set.toList $ fromList lst


randomInt :: Int -> Int
randomInt z = unsafePerformIO (getStdRandom (randomR (0, z - 1)))

remainingInputForError w = case w of
                  Error {remainingInput} -> Tx.length remainingInput + 1
                  Label {remainingInput} -> Tx.length remainingInput + 1

location n = take 5 . sort . uniquify . map ((n-) . remainingInputForError) . Data.SList.toList


closest :: Int -> [Int] -> Int
closest n = foldr min 1000000 . map (\x -> abs (x - n))


brokenFile :: IO (Tx.Text, Int, Int)
brokenFile =  do
    file <- T.readFile "tml.gcl"
    let k = 114 -- randomInt (Tx.length file)
    let (a, b) = Tx.splitAt k file
    let fixed = Tx.concat [a, Tx.tail b]
    return (fixed, k, Tx.length fixed)

tryAccu :: Int -> IO Int
tryAccu 0 = return 0
tryAccu n = do
  (broken, b, l) <- brokenFile
  case Accu.parse broken of
      (Left x) -> do
        let z = closest b (location l x)
        print "Accu reported"
        print $ location l x
        print "Accu original"
        print  b
        
        
        r <- tryAccu (n - 1)
        return (r + z)
      (Right _) -> tryAccu n

tryAttoParsec :: Int -> IO Int
tryAttoParsec 0 = return 0
tryAttoParsec n = do
  (broken, b, l) <- brokenFile
  case Atto.parseF broken of
      (Just x) -> do
        let dist = abs (l - (x + 1) - b) 
        print "Atto reported"
        print $  l - (x + 1)
        print "Atto original"
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
  return ()

