module Main(main) where

import Data.Foldable(for_)
import Data.List(isInfixOf)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Directory(listDirectory)
import System.Exit(exitFailure)

import GCL.Parser.Accuparsec qualified as GCL.Accu
import GCL.Parser.Attoparsec qualified as GCL.Atto
import JSON.Parser.Accuparsec qualified as JSON.Accu
import JSON.Parser.Attoparsec qualified as JSON.Atto

failWith :: FilePath -> String -> IO ()
failWith path msg = putStrLn (path <> ": " <> msg) *> exitFailure

test :: Eq a => String -> (Text -> Either e a) -> (Text -> Either String a) -> IO ()
test lang accu atto = do
  files <- listDirectory $ "progs/" <> lang

  for_ files \f -> do
    input <- T.readFile $ "progs/" <> lang <> "/" <> f

    case (accu input, atto input, "err" `isInfixOf` f) of
      (Right a, Right b, False)
        | a == b -> pure ()
        | otherwise -> failWith f "AST mismatch"
      (Left _, Left _, True) -> pure ()
      (Left _, Left _, False) -> failWith f "Both failed on valid input"
      (Right _, Right _, True) -> failWith f "Both succeeded on invalid input"
      _ -> failWith f "Result mismatch"

main :: IO ()
main = do
  test "gcl" GCL.Accu.parse GCL.Atto.parse
  test "json" JSON.Accu.parse JSON.Atto.parse
