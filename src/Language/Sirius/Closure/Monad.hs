{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Language.Sirius.Closure.Monad where

import qualified Control.Monad.RWS                          as RWS
import qualified Data.Set                                   as S
import qualified Language.Sirius.CST.Modules.Annoted        as C
import           Language.Sirius.Monomorphize               (runMGU)
import           Language.Sirius.Typecheck.ConstraintSolver (findM)
import qualified Language.Sirius.Typecheck.Definition.AST   as T
import qualified Language.Sirius.Typecheck.Definition.Type  as T

type MonadClosure m = (RWS.MonadRWS () [T.Toplevel] ClosureState m)

data ClosureState =
  ClosureState
    { clCounter   :: Int
    , clExcluded  :: S.Set (C.Annoted T.Type)
    , clToplevels :: [T.Toplevel]
    , clConverted :: S.Set Text
    }

fresh :: MonadClosure m => m Int
fresh = do
  i <- gets clCounter
  modify (\s -> s {clCounter = clCounter s + 1})
  return i

freshLambda :: MonadClosure m => m Text
freshLambda = do
  i <- fresh
  return $ "$$l" <> show i

freshEnv :: MonadClosure m => m Text
freshEnv = do
  i <- fresh
  return $ "$$e" <> show i

freshName :: MonadClosure m => m Text
freshName = do
  i <- fresh
  return $ "$$a" <> show i

isExcluded :: MonadClosure m => Text -> T.Type -> m Bool
isExcluded name ty = do
  e <- gets clExcluded
  isJust <$>
    findM
      (\case
         C.Annoted name' ty' -> do
           let bool' = name == name'
           bool'' <- runMGU ty ty'
           case bool'' of
             Left _          -> return False
             Right (Left _)  -> return False
             Right (Right _) -> return bool')
      e

addExcluded :: MonadClosure m => Text -> T.Type -> m ()
addExcluded name ty = do
  modify (\s -> s {clExcluded = S.insert (C.Annoted name ty) (clExcluded s)})

isToplevel :: MonadClosure m => Text -> m Bool
isToplevel name = do
  e <- gets clToplevels
  return $
    isJust $
    find
      (\case
         T.TStruct (C.Annoted name' _) _       -> name == name'
         T.TFunction _ (C.Annoted name' _) _ _ -> name == name'
         T.TExtern _ (C.Annoted name' _)       -> name == name'
         _                                     -> False)
      e

removeExcluded :: MonadClosure m => Text -> T.Type -> m ()
removeExcluded name ty = do
  modify (\s -> s {clExcluded = S.delete (C.Annoted name ty) (clExcluded s)})
