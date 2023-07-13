{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Language.Sirius.Typecheck.ConstraintSolver where

import           Language.Sirius.Typecheck.Definition.Monad
import           Language.Sirius.Typecheck.Modules.Apply
import           Language.Sirius.Typecheck.Modules.Substitution
import           Language.Sirius.Typecheck.Modules.Unification

import qualified Control.Monad.Except                           as E
import qualified Data.Bifunctor                                 as BF

import qualified Data.List                                      as L
import qualified Data.Map                                       as M
import qualified Language.Sirius.Typecheck.Definition.Type      as T
import           Prelude                                        hiding
                                                                (Constraint)
import qualified Control.Monad.State as ST
import qualified Data.Set as S

findM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

solve :: MonadChecker m => [Constraint] -> m Substitution
solve [] = return mempty
solve ((t1 :~: t2, pos):xs) = do
  s1 <- mgu t1 t2
  case s1 of
    Left err -> E.throwError (err, Nothing, pos)
    Right s1' -> do
      s2 <- solve $ map (BF.first (apply s1')) xs
      return $ s1' `compose` s2
solve ((Field f t1 t2, pos):xs) = do
  case t2 of
    T.TRec _ fields -> do
      case L.lookup f fields of
        Just t -> do
          s1 <- mgu t1 t
          case s1 of
            Left err -> E.throwError (err, Nothing, pos)
            Right s1' -> do
              s2 <- solve $ map (BF.first (apply s1')) xs
              return $ s1' `compose` s2
        Nothing -> solve xs
    T.TId n -> do
      types' <- gets types
      case M.lookup n types' of
        Just (T.Forall [] t) -> case t of
          T.TRec _ fields -> do
            case L.lookup f fields of
              Just t' -> do
                s1 <- mgu t1 t'
                case s1 of
                  Left err -> E.throwError (err, Nothing, pos)
                  Right s1' -> do
                    s2 <- solve $ map (BF.first (apply s1')) xs
                    return $ s1' `compose` s2
              Nothing -> solve xs
          _ -> E.throwError ("Expected record type, got " <> show t, Nothing, pos)
        _ ->
          E.throwError
            ("Expected record type, got " <> show t2, Nothing, pos)
    T.TApp (T.TId n) xs' -> do
      types' <- gets types
      case M.lookup n types' of
        Just (T.Forall gens t) -> do
          let sub = M.fromList $ zip gens xs'
          case apply sub t of
            T.TRec _ fields -> do
              case L.lookup f fields of
                Just t' -> do
                  s1 <- mgu t1 t'
                  case s1 of
                    Left err -> E.throwError (err, Nothing, pos)
                    Right s1' -> do
                      s2 <- solve $ map (BF.first (apply s1')) xs
                      return $ s1' `compose` s2
                Nothing -> solve xs
            _ -> E.throwError ("Expected record type, got " <> show t, Nothing, pos)
        Nothing ->
          E.throwError
            ("Expected record type, got " <> show t2, Nothing, pos)
    T.TVar _ ->
      E.throwError
        ("Expected a record, got a type variable " <> show t2, Nothing, pos)
    _ -> E.throwError ("Expected record type, got " <> show t2, Nothing, pos)
solve ((Class name ty (args T.:-> ret), pos):xs) = do
  classes' <- gets classes
  let classes'' = M.toList classes'
  classes3 <- sortClassesOnTrue classes''
  foundScheme <- find' (name, ty) classes3
  case foundScheme of
    Just scheme -> do
      t <- instantiate scheme
      s1 <- mgu t ((ty : args) T.:-> ret)
      case s1 of
        Left err -> E.throwError (err, Nothing, pos)
        Right s1' -> do
          s2 <- solve $ map (BF.first (apply s1')) xs
          return $ s1' `compose` s2
    Nothing ->
      E.throwError
        ( "Function property " <> name <> " not found on " <> show ty
        , Nothing
        , pos)
solve ((Hole t, pos):xs) = do
  ST.modify $ \s -> s {holes = S.insert (pos, t) $ holes s}
  solve xs

solve ((_, pos):_) = E.throwError ("Cannot solve constraints", Nothing, pos)

sortClassesOnTrue :: MonadChecker m => [((T.Type, Text, Bool), T.Scheme)] -> m [((T.Type, Text), T.Scheme)]
sortClassesOnTrue [] = return []
sortClassesOnTrue (((ty, name, True), sch):xs) = do
  rest <- sortClassesOnTrue xs
  return $ ((ty, name), sch) : rest
sortClassesOnTrue (((ty, n, False), sch):xs) = (++) <$> sortClassesOnTrue xs <*> pure [((ty, n), sch)]

find' :: MonadChecker m => (Text, T.Type) -> [((T.Type, Text), T.Scheme)] -> m (Maybe T.Scheme)
find' (txt, z@(T.TVar _)) (((T.TVar _, name), sch):xs) = do
  if txt == name
    then return $ Just sch
    else find' (txt, z) xs
find' (txt, ty) (((ty', name), sch):xs) = do
  if txt == name
    then do
      s <- mgu ty ty'
      case s of
        Left _  -> find' (txt, ty) xs
        Right _ -> return $ Just sch
    else find' (txt, ty) xs
find' _ [] = return Nothing