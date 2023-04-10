module Main(main) where

import Control.DeepSeq
import Criterion.Main

import Data.Text(Text)
import Data.Text.IO qualified as T

import GCL.Parser.Accuparsec qualified as Accu
import GCL.Parser.Attoparsec qualified as Atto

benchParser :: NFData a => (Text -> a) -> String -> Benchmark
benchParser f name =
  env (T.readFile $ "gcl/bench/progs/" <> name <> ".gcl")
  $ bench name . nf f

progs :: [String]
progs = ("prog" <>) . show <$> [10 :: Int, 20 .. 100]

main :: IO ()
main =
  defaultMain
  [ bgroup "accu" $ benchParser Accu.parse <$> progs
  , bgroup "atto" $ benchParser Atto.parse <$> progs
  ]
