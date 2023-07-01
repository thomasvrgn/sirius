module Language.Sirius.Typecheck.Modules.Substitution where

import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import           Language.Sirius.Typecheck.Definition.Type
import           Prelude                                   hiding (Type)

type Substitution = M.Map Int Type

class Types a where
  apply :: Substitution -> a -> a
  free :: a -> S.Set Int
