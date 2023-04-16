module Main(main) where

import Data.Foldable(for_)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory(createDirectoryIfMissing, removeDirectoryRecursive)

import GCL qualified
import JSON qualified

mkErr :: Text -> Text
mkErr = T.dropEnd 2

genProgs :: String -> (Int -> Text) -> [Int] -> IO ()
genProgs lang gen ns = do
  removeDirectoryRecursive $ "progs/" <> lang
  createDirectoryIfMissing True $ "progs/" <> lang

  for_ ns \n -> do
    let
      n' = show n

      prog = gen n
      progErr = mkErr prog
      size = show $ T.length prog

    T.writeFile ("progs/" <> lang <> "/" <> n' <> "-" <> size <>  "." <> lang) prog
    T.writeFile ("progs/" <> lang <> "/" <> n' <> "-err-" <> size <>  "." <> lang) progErr

main :: IO ()
main = do
  genProgs "gcl" GCL.genProg [20, 40, 60, 80]
  genProgs "json" JSON.genProg [100, 150, 200, 250]
