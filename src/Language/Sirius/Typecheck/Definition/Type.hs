{-# LANGUAGE PatternSynonyms #-}

module Language.Sirius.Typecheck.Definition.Type where

import qualified Data.List as L
import           Prelude   hiding (Type)
import qualified Text.Show as T

data Type
  = TVar Int
  | Int
  | Float
  | Void
  | Bool
  | Char
  | TId Text
  | TApp Type [Type]
  | TRec Text [(Text, Type)]
  deriving (Eq, Ord)

pattern (:->) :: [Type] -> Type -> Type

pattern (:->) a b = TApp (TApp (TId "->") a) [b]

pattern TTuple :: [Type] -> Type

pattern TTuple a = TApp (TId "Tuple") a

pattern TAddr :: Type -> Type

pattern TAddr a = TApp (TId "Address") [a]

pattern TString :: Type

pattern TString = TAddr Char

data Scheme =
  Forall [Int] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vs t) = "forall " ++ L.unwords (map show vs) ++ ". " ++ show t

instance T.Show Type where
  show (TVar i) = "a" ++ show i
  show Int = "Int"
  show Float = "Float"
  show Void = "Void"
  show Bool = "Bool"
  show Char = "Char"
  show (TAddr t) = "ref " ++ show t
  show (TId x) = toString x
  show (TTuple a) = "(" ++ intercalate ", " (map show a) ++ ")"
  show (t1 :-> t2) =
    "fn(" ++ intercalate ", " (map show t1) ++ "): " ++ show t2
  show (TApp t1 t2) = show t1 ++ "[" ++ L.intercalate ", " (map show t2) ++ "]"
  show (TRec n m) =
    toString n ++
    " {" ++
    intercalate ", " (map (\(k, v) -> toString k ++ ": " ++ show v) m) ++ "}"
