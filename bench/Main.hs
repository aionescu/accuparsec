module Main(main) where

import Criterion.Main
import Data.Char(isDigit)
import Data.List(sortOn)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory(listDirectory)
import Text.Printf(printf)

import GCL.Parser.Accuparsec qualified as GCL.Accu
import GCL.Parser.Attoparsec qualified as GCL.Atto
import JSON.Parser.Accuparsec qualified as JSON.Accu
import JSON.Parser.Attoparsec qualified as JSON.Atto
import System.FilePath ((</>))

readProgs :: FilePath -> IO [(String, Text)]
readProgs dir =
  traverse (fmap withSize . T.readFile . (dir </>))
  . sortOn (read @Int . takeWhile isDigit)
  =<< listDirectory dir
  where
    withSize t = size `seq` (size, t)
      where
        bytes = T.length t
        size
          | bytes < 1024 = show bytes <> " B"
          | bytes < 1024 * 1024 = bytes `fmtDiv` 1024 <> " KiB"
          | otherwise = bytes `fmtDiv` (1024 * 1024) <> " MiB"

fmtDiv :: Int -> Double -> String
fmtDiv d q = printf "%.2f" (fromIntegral d / q)

bench' :: (Text -> b) -> (String, Text) -> Benchmark
bench' parser (size, input) = bench size $ whnf parser input

main :: IO ()
main = do
  gclProgs <- readProgs "progs/gcl"
  jsonProgs <- readProgs "progs/json"

  defaultMain
    [ bgroup "gcl/accu" $ bench' GCL.Accu.parse <$> gclProgs
    , bgroup "gcl/atto" $ bench' GCL.Atto.parse <$> gclProgs
    , bgroup "json/accu" $ bench' JSON.Accu.parse <$> jsonProgs
    , bgroup "json/atto" $ bench' JSON.Atto.parse <$> jsonProgs
    ]
