{-# LANGUAGE FlexibleContexts #-}

module Language.Sirius.Typecheck.Modules.Unification where

import           Language.Sirius.Typecheck.Definition.Monad     (MonadChecker, CheckerState (types))
import           Language.Sirius.Typecheck.Definition.Type
import           Language.Sirius.Typecheck.Modules.Apply        (compose)
import           Language.Sirius.Typecheck.Modules.Substitution (Substitution,
                                                                 Types (apply, free))
import           Prelude                                        hiding (Type)

import qualified Control.Monad                                  as CM
import qualified Data.List                                      as L
import qualified Data.Map                                       as M
import           Data.Set                                       (member)

variable :: Int -> Type -> Either Text Substitution
variable n t
  | t == TVar n = Right M.empty
  | n `member` free t =
    Left $ "Occurs check failed in " <> show t <> " with " <> show (TVar n)
  | otherwise = Right $ M.singleton n t

mguScheme :: MonadChecker m => Scheme -> Scheme -> m (Either Text Substitution)
mguScheme (Forall _ t1) (Forall _ t2) = mgu t1 t2

mgu :: MonadChecker m => Type -> Type -> m (Either Text Substitution)
mgu (TVar i) t = return $ variable i t
mgu t (TVar i) = return $ variable i t
mgu Int Int = return $ Right M.empty
mgu Bool Bool = return $ Right M.empty
mgu Float Float = return $ Right M.empty
mgu Void Void = return $ Right M.empty
mgu Char Char = return $ Right M.empty
mgu (TId n) (TRec n' xs') =
  if n == n'
    then do
      found <- gets (M.lookup n . types)
      case found of
        Just (Forall [] t) -> mgu t (TRec n xs')
        _ -> return $ Left $ "Type " <> n <> " not found"
    else return $ Left $ "Type mismatch: " <> show n <> " and " <> show n'
mgu (TRec n xs') (TId n') =
  if n == n'
    then do
      found <- gets (M.lookup n . types)
      case found of
        Just (Forall [] t) -> mgu t (TRec n xs')
        _ -> return $ Left $ "Type " <> n <> " not found"
    else return $ Left $ "Type mismatch: " <> show n <> " and " <> show n'
mgu (TApp (TId n) xs) (TRec n' xs') =
  if n == n'
    then do
      found <- gets (M.lookup n . types)
      case found of
        Just (Forall gens t) -> do
          let sub = M.fromList $ zip gens xs
          mgu (apply sub t) (TRec n xs')
        Nothing -> return $ Left $ "Type " <> n <> " not found"
    else return $ Left $ "Type mismatch: " <> show n <> " and " <> show n'
mgu (TRec n xs') (TApp (TId n') xs) = do
  if n == n'
    then do
      found <- gets (M.lookup n . types)
      case found of
        Just (Forall gens t) -> do
          let sub = M.fromList $ zip gens xs
          mgu (apply sub t) (TRec n xs')
        Nothing -> return $ Left $ "Type " <> n <> " not found"
    else return $ Left $ "Type mismatch: " <> show n <> " and " <> show n'
mgu (TApp t1 t2) (TApp t3 t4) = mguMany (t1 : t2) (t3 : t4)
mgu (TId n) (TId n') =
  if n == n'
    then return $ Right M.empty
    else return $ Left $ "Type mismatch: " <> show n <> " and " <> show n'
mgu (TRec n1 f1) (TRec n2 f2) =
  if n1 == n2
    then CM.foldM
           (\s (f, t) ->
              case L.lookup f f2 of
                Just t' -> do
                  s' <- mgu t t'
                  return $ compose <$> s <*> s'
                Nothing -> return $ Right mempty)
           (Right M.empty)
           f1
    else return $ Left $ "Type mismatch: " <> show n1 <> " and " <> show n2
mgu s1 s2 =
  return $ Left $ "Type " <> show s1 <> " mismatches with type " <> show s2

mguMany :: MonadChecker m => [Type] -> [Type] -> m (Either Text Substitution)
mguMany [] [] = return $ Right M.empty
mguMany (t1:t1s) (t2:t2s) = do
  s1 <- mgu t1 t2
  case s1 of
    Left err -> return $ Left err
    Right s1' -> do
      s2 <- mguMany (apply s1' t1s) (apply s1' t2s)
      return $ compose s1' <$> s2
mguMany t1 t2 =
  return $ Left $ "Type mismatch: " <> show t1 <> " and " <> show t2
