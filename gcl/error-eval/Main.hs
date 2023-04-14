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


c :: Int -> Int
c z = unsafePerformIO (getStdRandom (randomR (0, z - 1)))

location x = take 1 (sort (uniquify $ map (\w -> case w of
                  Error {remainingInput} -> Tx.length remainingInput
                  Label {remainingInput} -> Tx.length remainingInput)
                  (Data.SList.toList x)))


closest :: Int -> [Int] -> Int
closest n = foldr min 1000000 . map (\x -> abs (x - n))

tryAccu :: Int -> IO Int
tryAccu 0 = return 0
tryAccu n = do
  z <- T.readFile "tml.gcl"
  print $ Tx.length z
  let k = 114 -- c (Tx.length z)
  let uu = (Tx.length z - k - 1)
  let (a, b) = Tx.splitAt k z
  let fixed = Tx.concat [a, Tx.tail b]
  case Accu.parse fixed of
      (Left x) -> do
        -- print $ location 
        let z = closest uu (location x)
        r <- tryAccu (n - 1)
        return (r + z)
      (Right _) -> tryAccu n

tryAttoParsec :: Int -> IO Int
tryAttoParsec 0 = return 0
tryAttoParsec n = do
  z <- T.readFile "tml.gcl"
  let k = 114 -- c (Tx.length z)
  let uu = (Tx.length z - k - 1)
  let (a, b) = Tx.splitAt k z
  let fixed = Tx.concat [a, Tx.tail b]
  case Atto.parseF fixed of
      (Just x) -> do
        -- print x
        let z = abs (x - uu)
        r <- tryAttoParsec (n - 1)
        return (r + z)
      Nothing -> tryAttoParsec n

main :: IO ()
main = do
  s <- tryAccu 1
  print s
  s <- tryAttoParsec 1
  print s


