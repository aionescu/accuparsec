module Main(main) where

import Data.Foldable(for_)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory(createDirectoryIfMissing)

import GCL qualified
import JSON qualified

mkErr :: Text -> Text
mkErr = T.dropEnd 2

main :: IO ()
main = do
  for_ ["progs/gcl", "progs/json"] $ createDirectoryIfMissing True

  for_ [30, 50, 70, 90] \n -> do
    let
      n' = show n

      gclProg = GCL.genProg n
      gclProgErr = mkErr gclProg
      gclSize = show $ T.length gclProg

      jsonProg = JSON.genProg n
      jsonProgErr = mkErr jsonProg
      jsonSize = show $ T.length jsonProg

    T.writeFile ("progs/gcl/" <> n' <> "-" <> gclSize <>  ".gcl") gclProg
    T.writeFile ("progs/gcl/" <> n' <> "-err-" <> gclSize <>  ".gcl") gclProgErr

    T.writeFile ("progs/json/" <> n' <> "-" <> jsonSize <>  ".json") jsonProg
    T.writeFile ("progs/json/" <> n' <> "-err-" <> jsonSize <>  ".json") jsonProgErr
