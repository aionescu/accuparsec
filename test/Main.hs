module Main(main) where

import Data.Foldable(for_)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Exit(exitFailure)

import GCL.Parser.Accuparsec qualified as GCL.Accu
import GCL.Parser.Attoparsec qualified as GCL.Atto
import JSON.Parser.Accuparsec qualified as JSON.Accu
import JSON.Parser.Attoparsec qualified as JSON.Atto

test :: Eq a => (Text -> Either e a) -> (Text -> Either String a) -> FilePath -> Int -> IO ()
test accu atto lang n = do
  let path = "progs/" <> lang <> "/" <> show n <> "." <> lang
  code <- T.readFile path
  case (accu code, atto code) of
    (Left _, Left _) -> pure ()
    (Right a, Right b) | a == b -> pure ()
    _ -> putStrLn ("Mismatch on " <> path) *> exitFailure

main :: IO ()
main = do
  for_ [10, 20 .. 100] $ test GCL.Accu.parse GCL.Atto.parse "gcl"
  for_ [10, 20 .. 100] $ test JSON.Accu.parse JSON.Atto.parse "json"
