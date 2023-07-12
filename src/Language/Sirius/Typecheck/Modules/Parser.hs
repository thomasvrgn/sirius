{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Language.Sirius.Typecheck.Modules.Parser where

import qualified Control.Monad.State                        as ST
import qualified Data.Map                                   as M
import qualified Language.Sirius.CST.Modules.Type           as D
import qualified Language.Sirius.Typecheck.Definition.Monad as M
import qualified Language.Sirius.Typecheck.Definition.Monad as T
import qualified Language.Sirius.Typecheck.Definition.Type  as T
import qualified Language.Sirius.CST.Modules.Namespaced as D
import qualified Language.Sirius.Typecheck.Modules.Substitution as U

class Parser a where
  to :: T.MonadChecker m => a -> m T.Type
  to = (`toWithEnv` mempty)
  toWithEnv :: T.MonadChecker m => a -> M.Map Text T.Type -> m T.Type
  from :: T.Type -> a

withGenerics :: T.MonadChecker m => M.Map Text T.Type -> m a -> m a
withGenerics xs m = do
  ST.modify (\s -> s {M.generics = xs `M.union` M.generics s})
  a <- m
  ST.modify (\s -> s {M.generics = M.difference (M.generics s) xs})
  return a

instance Parser a => Parser (Maybe a) where
  toWithEnv x env =
    case x of
      Just x' -> toWithEnv x' env
      Nothing -> T.fresh
  from = error "from: Maybe"

instance Parser D.Type where
  toWithEnv = go
    where
      go :: T.MonadChecker m => D.Type -> M.Map Text T.Type -> m T.Type
      go (D.TypeApp (D.Simple v) xs) env = do
        ts <- mapM (`go` env) xs
        aliases <- ST.gets T.aliases
        case M.lookup v aliases of
          Just (T.Forall gens t) -> do
            let env' = M.fromList (zip gens ts)
            return (U.apply env' t)
          _ -> if null xs
            then return $ T.TId v
            else do
              return (T.TApp (T.TId v) ts)
      go (D.TypeVar (D.Simple v)) env = do
        case M.lookup v env of
          Just t -> return t
          Nothing -> do
            env' <- ST.gets T.types
            case M.lookup v env' of
              Just _ -> return $ T.TId v
              Nothing -> do
                aliases <- ST.gets T.aliases
                case M.lookup v aliases of
                  Just (T.Forall gens t) -> do
                    if null gens then
                      return t
                    else error "go: TypeVar"
                  Nothing -> T.fresh
      go D.TypeInt _ = return T.Int
      go D.TypeBool _ = return T.Bool
      go D.TypeFloat _ = return T.Float
      go D.TypeChar _ = return T.Char
      go D.TypeVoid _ = return T.Void
      go (D.TypeFunction args ret) env = do
        args' <- mapM (`go` env) args
        ret' <- go ret env
        return (args' T.:-> ret')
      go _ _ = error "go: Type"
  from = error "from: Type"
