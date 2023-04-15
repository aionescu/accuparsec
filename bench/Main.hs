module Main(main) where

import Criterion.Main
import Data.List(sort)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Directory(listDirectory)

import GCL.Parser.Accuparsec qualified as GCL.Accu
import GCL.Parser.Attoparsec qualified as GCL.Atto
import JSON.Parser.Accuparsec qualified as JSON.Accu
import JSON.Parser.Attoparsec qualified as JSON.Atto

benchProg :: (Text -> a) -> FilePath -> FilePath -> Benchmark
benchProg parser lang file =
  env (T.readFile path) $ bench file . whnf parser
  where
    path = "progs/" <> lang <> "/" <> file

main :: IO ()
main = do
  gclProgs <- sort <$> listDirectory "progs/gcl"
  jsonProgs <- sort <$> listDirectory "progs/json"

  defaultMain
    [ bgroup "gcl/accu" $ benchProg GCL.Accu.parse "gcl" <$> gclProgs
    , bgroup "gcl/atto" $ benchProg GCL.Atto.parse "gcl" <$> gclProgs
    , bgroup "json/accu" $ benchProg JSON.Accu.parse "json" <$> jsonProgs
    , bgroup "json/atto" $ benchProg JSON.Atto.parse "json" <$> jsonProgs
    ]
