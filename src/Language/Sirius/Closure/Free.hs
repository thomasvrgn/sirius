{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
module Language.Sirius.Closure.Free where
import qualified Data.Set as S
import Language.Sirius.Typecheck.Definition.AST
import qualified Language.Sirius.CST.Modules.Annoted as C
import qualified Language.Sirius.Typecheck.Definition.Type as T
import Data.Foldable (Foldable(foldl))

class Free a where
  free :: a -> S.Set (C.Annoted T.Type)

instance Free a => Free [a] where
  free = S.unions . map free

instance Free a => Free (Maybe a) where
  free = maybe S.empty free

instance Free a => Free (Either a b) where
  free = either free (const S.empty)

instance Free a => Free (b, a) where
  free = free . snd

instance Free a => Free (C.Annoted a) where
  free = free . C.annotedType

instance Free Expression where
  free :: Expression -> Set (C.Annoted T.Type)
  free (EVariable x t) = S.singleton (C.Annoted x t)
  free (EApplication f args _) = free f `S.union` free args
  free (EIf cond t f) = free cond `S.union` free t `S.union` free f
  free (EList xs _) = free xs
  free (EIndex arr idx) = free arr `S.union` free idx
  free (ELiteral _) = S.empty
  free (EFunction _ args body) = free body S.\\ S.fromList args
  free (EProperty x _) = free x
  free (EStruct _ xs) = free xs
  free (EUpdate x e) = free x `S.union` free e
  free (ELet n e b _) = (free e `S.union` free b) S.\\ S.singleton n
  free (EDereference x) = free x
  free (EBlock xs) = freeBody xs
  free (EReference e) = free e
  free (EWhile cond body) = free cond `S.union` free body
  free (EFor n from to body) = free from `S.union` free to `S.union` (free body S.\\ S.singleton n)
  free (ESizeOf _) = S.empty
  free (EAssembly _ xs) = free xs
  free (ELocated e _) = free e
  free _ = S.empty

instance Free UpdateExpression where
  free (UVariable name ty) = S.singleton (C.Annoted name ty)
  free (UIndex arr idx) = free arr `S.union` free idx
  free (UProperty x _) = free x
  free (UDereference x) = free x

instance Free Toplevel where
  free (TFunction _ name args body) = free body S.\\ S.fromList (name:args)
  free _ = S.empty

freeBody :: [Expression] -> S.Set (C.Annoted T.Type)
freeBody body = freeBodyExcl body S.empty

freeBodyExcl :: [Expression] -> S.Set (C.Annoted T.Type) -> S.Set (C.Annoted T.Type)
freeBodyExcl body excl = fst $ foldl (\(acc, excluded) -> \case
    ELet n e b _ -> 
      ((acc `S.union` free e `S.union` free b) S.\\ S.singleton n, excluded `S.union` S.singleton n)
    x -> 
      ((acc `S.union` free x) S.\\ excluded, excluded)) (S.empty, excl) body