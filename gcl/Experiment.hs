{-# options_ghc
  -Wno-unused-imports
#-}

{-# language
  QuasiQuotes
#-}

module Experiment where

import Text.RawString.QQ (r)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal qualified as TI
import GCL.Parser.Attoparsec qualified as GT
import GCL.Parser.Accuparsec qualified as GC
-- import GCL.Parser.Rednaz qualified as GR
import Data.Accuparsec.Text qualified as LC
import Data.Accuparsec.Text (ParseError (Error, Label, expected))
import Data.Attoparsec.Text qualified as LT
-- import Text.Parse.Char.AttoparsecInterface qualified as LR
import Data.List qualified as L
import Data.SList qualified
import Data.List.NonEmpty (NonEmpty ((:|)), groupWith)
import Data.Foldable (Foldable (toList))

t :: Text
t = [r|main () -> a: Int {
a;
}
|]

-- $> parseT (GT.ws *> GT.program <* LT.endOfInput) t

-- $> parseC (GC.ws *> GC.program <* LC.endOfInput) t

-- -- $> parseR (GR.ws *> GR.program <* LR.endOfInput) t

data GroupedParseError =
  GroupedParseError
    Text -- ^ expected
  |
  GroupedFail
    Text -- ^ expected
  deriving stock (Eq, Show)

groupErrors :: LC.ErrorList -> [(Text, NonEmpty GroupedParseError)]
groupErrors =
  fmap (\es@(eHead :| _) -> (inputRestGet eHead, toGrouped <$> es)) .
  groupWith (fastLength . inputRestGet) .
  L.sortOn (fastLength . inputRestGet) .
  Data.SList.toList

parseT :: LT.Parser a -> Text -> LT.Result a
parseT =
  (\case
    r@(LT.Fail {}) -> r
    r@(LT.Done {}) -> r
    LT.Partial f -> f ""
  )
  .:
  LT.parse

parseC :: (Show a) => LC.Parser a -> Text -> IO ()
parseC =
  putStr .:
  either
    (errorBundlePretty . groupErrors)
    show
  .:
  LC.runParser

-- parseR :: (Show a) => LR.Parser a -> Text -> IO ()
-- parseR =
--   putStr .:
--   either
--     LR.errorBundlePretty
--     show
--   .:
--   LR.runParser

-- to do. calculate line and column numbers
errorBundlePretty :: [(Text, NonEmpty GroupedParseError)] -> String
errorBundlePretty =
  foldMap
    (\(actual, errors) ->
      "unexpected " <>
      (show $ T.unlines $ take 5 $ T.lines $ actual) <> "\n" <>
      "expecting " <> show (toList errors) <> "\n\n"
    )

inputRestGet :: ParseError -> Text
inputRestGet = LC.remainingInput

toGrouped :: ParseError -> GroupedParseError
toGrouped (Error {expected}) = GroupedParseError expected
toGrouped (Label {label}) = GroupedFail label
{-# inline toGrouped #-}

fastLength :: Text -> Int
fastLength (TI.Text _ _ l) = l
{-# inline fastLength #-}

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
