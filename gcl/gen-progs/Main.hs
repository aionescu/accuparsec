module Main(main) where

import Data.Foldable(for_)
import Data.Functor((<&>))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

showT :: Show a => a -> Text
showT = T.pack . show

genExpr :: Int -> Text
genExpr n = T.intercalate " + " $ replicate n $ T.intercalate " * " $ [1..n] <&> \n -> "x" <> showT n

genStmt :: Int -> Text
genStmt n = "      y = y + " <> genExpr n <> ";\n"

genStmts :: Int -> Text
genStmts n = foldMap genStmt [1..n]

genInputs :: Int -> Text
genInputs n = T.intercalate ", " $ [1..n] <&> \n -> "x" <> showT n <> ": Int"

genCond :: Int -> Text
genCond n = T.intercalate " && " $ [1..n] <&> \n -> "x" <> showT n <> " > 0"

genAssume :: Int -> Text
genAssume n = "  assume " <> genCond n <> ";\n"

genAssert :: Int -> Text
genAssert n = "  assert y == " <> T.intercalate " + " (genExpr <$> [1..n]) <> ";\n"

genProgram :: Int -> Text
genProgram n =
  "-- Program intended for parser benchmarks, with n = " <> showT n <> "\n"
  <> "prog" <> showT n <> "(" <> genInputs n <> ") -> y: Int {\n"
  <> genAssume n
  <> "  y = 0;\n"
  <> "  let b: Bool {\n"
  <> "    b = True;\n"
  <> "    while b && " <> genCond n <> " {\n"
  <> genStmts n
  <> "      b = False;\n"
  <> "    }\n"
  <> "  }\n"
  <> genAssert n
  <> "}\n"

main :: IO ()
main = do
  for_ [10 :: Int, 20 .. 100] \n ->
    T.writeFile ("gcl/bench/progs/prog" <> show n <> ".gcl")
    $ genProgram n
