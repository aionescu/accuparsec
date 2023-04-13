module Main(main) where

import Data.Foldable(for_)
import Data.Text.IO qualified as T
import System.Directory(createDirectoryIfMissing)

import GCL qualified
import JSON qualified

main :: IO ()
main = do
  createDirectoryIfMissing True "progs/gcl"
  createDirectoryIfMissing True "progs/json"

  for_ [10 :: Int, 20 .. 100] \n -> do
    T.writeFile ("progs/gcl/" <> show n <> ".gcl") $ GCL.genProg n
    T.writeFile ("progs/json/" <> show n <> ".json") $ JSON.genProg n
