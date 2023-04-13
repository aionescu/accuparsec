module Main(main) where

import Data.Foldable(traverse_)
import Data.Text.IO qualified as T
import System.Exit(exitFailure)

import Language.GCL.Parser.Accuparsec qualified as Accu
import Language.GCL.Parser.Attoparsec qualified as Atto

test :: FilePath -> IO ()
test path = do
  code <- T.readFile $ "progs/" <> path <> ".gcl"
  case (Accu.parse code, Atto.parse code) of
    (Left _, Left _) -> pure ()
    (Right a, Right b) | a == b -> pure ()
    _ -> putStrLn ("Mismatch on " <> path) *> exitFailure

main :: IO ()
main = traverse_ test $ ("prog" <>) . show <$> [10 :: Int, 20 .. 100]
