module Main(main) where

import Criterion.Main

import Data.Text(Text)
import Data.Text.IO qualified as T

import Language.GCL.Parser.Accuparsec qualified as Accu
import Language.GCL.Parser.Attoparsec qualified as Atto

benchParser :: (Text -> a) -> String -> Benchmark
benchParser f name =
  env (T.readFile $ "progs/" <> name <> ".gcl")
  $ bench name . whnf f

progs :: [String]
progs = ("prog" <>) . show <$> [10 :: Int, 20 .. 100]

main :: IO ()
main =
  defaultMain
  [ bgroup "accu" $ benchParser Accu.parse <$> progs
  , bgroup "atto" $ benchParser Atto.parse <$> progs
  ]
