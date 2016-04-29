module JSONAST.JSONEncoder
where

import BasePrelude hiding (null)
import JSONEncoder
import JSONAST
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.HashMap.Strict (HashMap)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Contravariant.Extras
import qualified Data.HashMap.Strict as HashMap


json :: Value JSON
json =
  contramap match $
  chosen jsonObject $
  chosen jsonArray $
  chosen jsonString $
  chosen jsonNumber $
  chosen jsonBoolean $
  jsonNull
  where
    match =
      \case
        JSON_Object x -> Left x
        JSON_Array x -> Right (Left x)
        JSON_String x -> Right (Right (Left x))
        JSON_Number x -> Right (Right (Right (Left x)))
        JSON_Bool x -> Right (Right (Right (Right (Left x))))
        _ -> Right (Right (Right (Right (Right ()))))

jsonObject :: Value (HashMap Text JSON)
jsonObject =
  object $
  contramap HashMap.toList $
  contramany $
  row json

jsonArray :: Value (Vector JSON)
jsonArray =
  array $
  homo foldl' json

jsonString :: Value Text
jsonString =
  string

jsonNumber :: Value Scientific
jsonNumber =
  number_scientific

jsonBoolean :: Value Bool
jsonBoolean =
  boolean

jsonNull :: Value ()
jsonNull =
  null

