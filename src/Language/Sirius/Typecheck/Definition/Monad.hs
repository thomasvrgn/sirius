{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Language.Sirius.Typecheck.Definition.Monad where

import           Control.Monad.Except                           (MonadError)
import qualified Data.Map                                       as M
import qualified Language.Sirius.CST.Modules.Located            as C
import qualified Language.Sirius.Typecheck.Definition.Type      as T
import qualified Language.Sirius.Typecheck.Modules.Apply        as A
import           Language.Sirius.Typecheck.Modules.Substitution (Types (apply))
import           Prelude                                        hiding
                                                                (Constraint,
                                                                 Type, ask,
                                                                 local)
import qualified Data.Set as S

type Environment = Map Text T.Scheme

type Envs = (Environment, Environment)

type Infer f m f'
   = MonadChecker m =>
       f -> m (T.Type, f')

data CheckerState =
  CheckerState
    { counter     :: Int
    , variables   :: Environment
    , types       :: Environment
    , constraints :: [A.Constraint]
    , generics    :: M.Map Text T.Type
    , returnType  :: T.Type
    , classes     :: M.Map (T.Type, Text, Bool) T.Scheme
    , aliases     :: M.Map Text T.Scheme
    , holes       :: S.Set (C.Position, T.Type)
    }
  
instance Semigroup CheckerState where
  (<>) s1 s2 =
    CheckerState
      { counter = if counter s1 > counter s2 then counter s1 else counter s2
      , variables = variables s1 `M.union` variables s2
      , types = types s1 `M.union` types s2
      , constraints = constraints s1 ++ constraints s2
      , generics = generics s1 `M.union` generics s2
      , returnType = if returnType s1 == T.Void then returnType s2 else returnType s1
      , classes = classes s1 `M.union` classes s2
      , aliases = aliases s1 `M.union` aliases s2
      , holes = holes s1 `S.union` holes s2
      }

instance Monoid CheckerState where
  mempty =
    CheckerState
      { counter = 0
      , variables = mempty
      , types = mempty
      , constraints = mempty
      , generics = mempty
      , returnType = T.Void
      , classes = mempty
      , aliases = mempty
      , holes = mempty
      }

union' :: Envs -> Envs -> Envs
union' (vars1, types1) (vars2, types2) =
  (vars1 `M.union` vars2, types1 `M.union` types2)

type MonadChecker m
   = (MonadState CheckerState m, MonadError (Text, Maybe Text, C.Position) m)

fresh :: MonadChecker m => m T.Type
fresh =
  gets counter >>= \n -> modify (\s -> s {counter = n + 1}) >> return (T.TVar n)

instantiate :: MonadChecker m => T.Scheme -> m T.Type
instantiate (T.Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = fromList $ zip vars vars'
   in return $ apply s t

ask :: MonadChecker m => m Envs
ask = (,) <$> gets variables <*> gets types

local :: MonadChecker m => (Envs -> Envs) -> m a -> m a
local f m = do
  (vars, types') <- ask
  let (vars', types'') = f (vars, types')
  modify (\s -> s {variables = vars', types = types''})
  a <- m
  cnt' <- gets counter
  csts <- gets constraints
  holes' <- gets holes
  modify
    (\s ->
       s {variables = vars, types = types', counter = cnt', constraints = csts, holes = holes'})
  return a

local' :: MonadChecker m => m a -> m a
local' = local id
