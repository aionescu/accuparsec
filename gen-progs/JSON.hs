module JSON(genProg) where

import Data.Text(Text)
import Data.Text qualified as T

showT :: Show a => a -> Text
showT = T.pack . show

showB :: Bool -> Text
showB True = "true"
showB False = "false"

genUser :: Int -> Text
genUser n =
  "{ \"name\": \"User #" <> showT n
  <> "\", \"id\": " <> showT n
  <> ", \"isEven\": " <> showB (even n)
  <> ", \"coworkers\": [" <> T.intercalate ", " (genUser <$> [1 .. n `quot` 2]) <> "]"
  <> " }"

genProg :: Int -> Text
genProg n =
  "// JSON data file intended for parser benchmarks, with n = " <> showT n <> "\n"
  <> "{\n  \"users\": [\n    "
  <> T.intercalate ",\n    " (genUser <$> [1 .. n])
  <> "\n  ]\n}\n"
