module Language.Sirius.CST.Modules.Type where

import           Prelude hiding (Type)
import Language.Sirius.CST.Modules.Namespaced (Namespaced)

data Type
  = TypeInt
  | TypeBool
  | TypeChar
  | TypeVoid
  | TypeFloat
  -- ^ TypeInt denotes the type int
  --   TypeBool denotes the type bool
  --   TypeChar denotes the type char
  --   TypeString denotes the type string
  --   TypeVoid denotes the type void
  --   TypeFloat denotes the type float
  | TypeApp Namespaced [Type]
  -- ^ TypeApp denotes a type application, e.g. User[int]
  --   The first argument is the name of the type constructor
  --   The second argument is a list of type arguments
  --   For example, List[Int] is represented as
  --   TypeApp "List" [TypeInt]
  | TypeVar Namespaced
  -- ^ TypeVar denotes a type variable or a type identifier, e.g. T
  --   The argument is the name of the type variable
  --   For example, T is represented as
  --   TypeVar "T"
  | TypeFunction [Type] Type
  deriving (Eq, Show)
