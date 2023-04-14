module Main(main) where

import Criterion.Main

import Data.Text(Text)
import Data.Text.IO qualified as T

import GCL.Parser.Accuparsec qualified as GCL.Accu
import GCL.Parser.Attoparsec qualified as GCL.Atto
import JSON.Parser.Accuparsec qualified as JSON.Accu
import JSON.Parser.Attoparsec qualified as JSON.Atto

benchProg :: (Text -> a) -> FilePath -> Int -> Benchmark
benchProg parser lang (show -> n) =
  env (T.readFile path) $ bench n . whnf parser
  where
    path = "progs/" <> lang <> "/" <> n <> "." <> lang

main :: IO ()
main =
  defaultMain
  [ bgroup "gcl/accu" $ benchProg GCL.Accu.parse "gcl" <$> [10, 20 .. 100]
  , bgroup "gcl/atto" $ benchProg GCL.Atto.parse "gcl" <$> [10, 20 .. 100]
  , bgroup "json/accu" $ benchProg JSON.Accu.parse "json" <$> [10, 20 .. 100]
  , bgroup "json/atto" $ benchProg JSON.Atto.parse "json" <$> [10, 20 .. 100]
  ]
