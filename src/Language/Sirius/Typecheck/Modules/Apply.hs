{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Sirius.Typecheck.Modules.Apply where

import qualified Data.Map                                       as M
import qualified Data.Set                                       as S
import qualified Language.Sirius.CST.Modules.Annoted            as C
import           Language.Sirius.CST.Modules.Located
import           Language.Sirius.Typecheck.Definition.AST
import qualified Language.Sirius.Typecheck.Definition.Type      as T
import           Language.Sirius.Typecheck.Modules.Substitution
import           Prelude                                        hiding
                                                                (Constraint)
import qualified Text.Show                                      as T

type Variables = M.Map Text T.Type

type Environment = (Variables, Variables)

data ConstraintConstructor
  = T.Type :~: T.Type
  | Field Text T.Type T.Type
  | Class Text T.Type T.Type

type Constraint = (ConstraintConstructor, Position)

instance Types T.Scheme where
  free (T.Forall vars t) = free t `S.difference` S.fromList vars
  apply s (T.Forall vars t) = T.Forall vars $ apply (foldr M.delete s vars) t

instance {-# OVERLAPPING #-} Types Constraint where
  free (c, _) = free c
  apply s (c, pos) = (apply s c, pos)

instance T.Show ConstraintConstructor where
  show (t1 :~: t2)     = show t1 ++ " ~ " ++ show t2
  show (Field f t1 t2) = show t1 ++ "." ++ toString f ++ " ~ " ++ show t2
  show (Class n t1 t2) = show t1 ++ " : " ++ toString n ++ " " ++ show t2

instance {-# OVERLAPS #-} T.Show Constraint where
  show (c, _) = show c

instance Types ConstraintConstructor where
  free (t1 :~: t2)     = free t1 `S.union` free t2
  free (Field _ t1 t2) = free t1 `S.union` free t2
  free (Class _ t1 t2) = free t1 `S.union` free t2
  apply s (t1 :~: t2)     = apply s t1 :~: apply s t2
  apply s (Field f t1 t2) = Field f (apply s t1) (apply s t2)
  apply s (Class n t1 t2) = Class n (apply s t1) (apply s t2)

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.map (apply s1) s2 `M.union` M.map (apply s2) s1

instance Types T.Type where
  free (T.TVar i)    = S.singleton i
  free T.Int         = S.empty
  free (T.TApp n xs) = free n `S.union` free xs
  free T.Bool        = S.empty
  free T.Float       = S.empty
  free T.Void        = S.empty
  free (T.TId _)     = S.empty
  free T.Char        = S.empty
  free (T.TRec _ xs) = free xs
  apply s (T.TVar i) =
    case M.lookup i s of
      Just t  -> t
      Nothing -> T.TVar i
  apply s (T.TApp n xs) = T.TApp (apply s n) $ apply s xs
  apply _ T.Int = T.Int
  apply _ T.Bool = T.Bool
  apply _ T.Float = T.Float
  apply _ T.Void = T.Void
  apply _ (T.TId s) = T.TId s
  apply _ T.Char = T.Char
  apply s (T.TRec n xs) = T.TRec n $ apply s xs

instance {-# OVERLAPPING #-} Types a => Types (M.Map Text a) where
  free = free . M.elems
  apply s = M.map (apply s)

instance Types Char where
  free _ = S.empty
  apply _ = id

instance Types Text where
  free _ = S.empty
  apply _ = id

instance Types a => Types [a] where
  free = foldr (S.union . free) S.empty
  apply s = map (apply s)

instance Types a => Types (Located a) where
  free (a :>: _) = free a
  free _         = S.empty
  apply s (a :>: pos) = apply s a :>: pos
  apply _ x           = x

instance Types a => Types (Maybe a) where
  free = maybe S.empty free
  apply s = fmap (apply s)

instance Types a => Types (Either a b) where
  free = either free (const S.empty)
  apply s = either (Left . apply s) Right

instance (Types a, Types b) => Types (a, b) where
  free = S.union <$> free . fst <*> free . snd
  apply s (a, b) = (apply s a, apply s b)

instance Types Position where
  free _ = S.empty
  apply _ = id

instance Types Expression where
  free (EVariable _ t) = free t
  free (EApplication f xs t) = free f `S.union` free xs `S.union` free t
  free (EFunction ret args e) =
    free ret `S.union` free args `S.union` free e
  free (ELet name e1 e2 t) = free name `S.union` free e1 `S.union` free e2 `S.union` free t
  free (EBlock xs) = free xs
  free (EList es t) = free es `S.union` free t
  free (EIndex e i) = free e `S.union` free i
  free (EIf e1 e2 e3) = free e1 `S.union` free e2 `S.union` free e3
  free (EUpdate x e) = free x `S.union` free e
  free (EStruct n xs) = free n `S.union` free xs
  free (EProperty e _) = free e
  free (EClassVariable _ t app) = free t `S.union` free app
  free (EReference e) = free e
  free (ELiteral _) = S.empty
  free (EDereference e) = free e
  free (EWhile e1 e2) = free e1 `S.union` free e2
  free (EFor name e1 e2 e3) = free name `S.union` free e1 `S.union` free e2 `S.union` free e3
  free (ESizeOf t) = free t
  free (ELocated e _) = free e
  free (EAssembly _ args) = free args
  free (EDeclaration _ t) = free t

  apply s (EVariable name t) = EVariable name $ apply s t
  apply s (EApplication f xs t) = EApplication (apply s f) (apply s xs) (apply s t)
  apply s (EFunction ret args e) =
    EFunction (apply s ret) (apply s args) $ apply s e
  apply s (ELet name e1 e2 t) = ELet (apply s name) (apply s e1) (apply s e2) (apply s t)
  apply s (EBlock xs) = EBlock $ apply s xs
  apply s (EList es t) = EList (apply s es) (apply s t)
  apply s (EIndex e i) = EIndex (apply s e) $ apply s i
  apply s (EIf e1 e2 e3) = EIf (apply s e1) (apply s e2) $ apply s e3
  apply s (EUpdate x e) = EUpdate (apply s x) $ apply s e
  apply _ (ELiteral l) = ELiteral l
  apply s (EStruct n fields) = EStruct (apply s n) $ apply s fields
  apply s (EProperty e f) = EProperty (apply s e) f
  apply s (EClassVariable name t app) = EClassVariable name (apply s t) (apply s app)
  apply s (EDereference e) = EDereference $ apply s e
  apply s (EReference e) = EReference $ apply s e
  apply s (EWhile e1 e2) = EWhile (apply s e1) $ apply s e2
  apply s (EFor name e1 e2 e3) =
    EFor (apply s name) (apply s e1) (apply s e2) $ apply s e3
  apply s (ESizeOf t) = ESizeOf $ apply s t
  apply s (ELocated e pos) = ELocated (apply s e) pos
  apply s (EAssembly op es) = EAssembly op $ apply s es
  apply s (EDeclaration name t) = EDeclaration name (apply s t)

instance Types UpdateExpression where
  free (UVariable _ e) = free e
  free (UIndex x y) = free x `S.union` free y
  free (UProperty x _) = free x
  free (UDereference x) = free x
  
  apply s (UVariable name e) = UVariable name $ apply s e
  apply s (UIndex x y)       = UIndex (apply s x) $ apply s y
  apply s (UProperty x f)    = UProperty (apply s x) f
  apply s (UDereference x)   = UDereference $ apply s x

instance Types Toplevel where
  free _ = error "free not implemented for Toplevel"
  apply s (TFunction gens name args body) =
    TFunction (apply s gens) (apply s name) (apply s args) (apply s body)
  apply s (TExtern gens name) = TExtern (apply s gens) (apply s name)
  apply s (TStruct name fields) = TStruct (apply s name) (apply s fields)
  apply s (TFunctionProp gens prop name args body) =
    TFunctionProp
      (apply s gens)
      (apply s prop)
      (apply s name)
      (apply s args)
      (apply s body)
  apply s (TEnumeration name fields) =
    TEnumeration (apply s name) (apply s fields)
  apply _ z@(TUnion _ _) = z

instance Types a => Types (C.Annoted a) where
  free = free . C.annotedType
  apply s (C.Annoted x t) = C.Annoted x (apply s t)
