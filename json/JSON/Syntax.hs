module JSON.Syntax where

import Data.Map.Strict(Map)
import Data.Text(Text)

data JSON
  = Null
  | Bool Bool
  | Number Integer
  | String Text
  | Array [JSON]
  | Object (Map Text JSON)
  deriving stock (Eq, Show)
