{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Sirius.Memory.Management where

import qualified Control.Monad.RWS                      as RWS
import qualified Control.Monad.State                    as ST
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Language.Sirius.CST.Expression         as C
import qualified Language.Sirius.CST.Modules.Annoted    as C
import qualified Language.Sirius.CST.Modules.Located    as C
import qualified Language.Sirius.CST.Modules.Namespaced as D
import qualified Language.Sirius.CST.Modules.Type       as T
import           Prelude                                hiding (local)

malloc :: [Text]
malloc = ["malloc", "ref_malloc"]

type MonadMemory m = (RWS.MonadRWS Bool () ([Text], [Text], M.Map Text Int) m)

getVariableAllocationReturned :: [Text] -> C.Located C.Expression -> [Text]
getVariableAllocationReturned graphs (C.EVariable (D.Simple name) C.:>: _) =
  [name | name `elem` graphs]
getVariableAllocationReturned graphs (C.EIf _ xs ys C.:>: _) =
  case (viaNonEmpty last xs, viaNonEmpty last ys) of
    (Just x, Just y) ->
      getVariableAllocationReturned graphs x <>
      getVariableAllocationReturned graphs y
    _ -> []
getVariableAllocationReturned graphs (C.EBlock xs C.:>: _) =
  maybe [] (getVariableAllocationReturned graphs) (viaNonEmpty last xs)
getVariableAllocationReturned graphs (C.EApplication (C.EVariable (D.Simple name) C.:>: _) xs C.:>: _) =
  concatMap (getVariableAllocationReturned graphs) xs <>
  [name | name `elem` graphs]
getVariableAllocationReturned graphs (C.EApplication f xs C.:>: _) =
  getVariableAllocationReturned graphs f <>
  concatMap (getVariableAllocationReturned graphs) xs
getVariableAllocationReturned graphs (C.EAssembly name xs C.:>: _) =
  concatMap (getVariableAllocationReturned graphs) xs <>
  [name | name `elem` graphs]
getVariableAllocationReturned graphs (C.EProperty e name C.:>: _) =
  getVariableAllocationReturned graphs e <> [name | name `elem` graphs]
getVariableAllocationReturned graphs (C.EStruct _ xs C.:>: _) =
  concatMap (getVariableAllocationReturned graphs . C.annotedType) xs
getVariableAllocationReturned graphs (C.EList xs C.:>: _) =
  concatMap (getVariableAllocationReturned graphs) xs
getVariableAllocationReturned graphs (C.EIndex e1 e2 C.:>: _) =
  getVariableAllocationReturned graphs e1 <>
  getVariableAllocationReturned graphs e2
getVariableAllocationReturned graphs (C.EDereference e C.:>: _) =
  getVariableAllocationReturned graphs e
getVariableAllocationReturned graphs (C.EReference e C.:>: _) =
  getVariableAllocationReturned graphs e
getVariableAllocationReturned _ (C.ESizeOf _ C.:>: _) = []
getVariableAllocationReturned graphs (C.EWhile e xs C.:>: _) =
  getVariableAllocationReturned graphs e <>
  concatMap (getVariableAllocationReturned graphs) xs
getVariableAllocationReturned graphs (C.EFor _ e1 e2 xs C.:>: _) =
  getVariableAllocationReturned graphs e1 <>
  getVariableAllocationReturned graphs e2 <>
  concatMap (getVariableAllocationReturned graphs) xs
getVariableAllocationReturned graphs (C.ELet _ e b C.:>: _) =
  getVariableAllocationReturned graphs e <>
  maybe [] (getVariableAllocationReturned graphs) b
getVariableAllocationReturned graphs (C.EUpdate _ e C.:>: _) =
  getVariableAllocationReturned graphs e
getVariableAllocationReturned _ _ = []

analyseToplevel :: MonadMemory m => C.Toplevel -> m C.Toplevel
analyseToplevel (C.TAnnotation "no-reference-counting" (C.Located _ t)) =
  return t
analyseToplevel (C.TFunction gens name args xs) = do
  xs' <- analyseExpression xs
  return $ C.TFunction gens name args xs'
analyseToplevel (C.TFunctionProp gens prop name args xs) = do
  xs' <- analyseExpression xs
  return $ C.TFunctionProp gens prop name args xs'
analyseToplevel x = return x

local' :: MonadMemory m => m a -> m a
local' m = do
  (vars, malloc', t) <- ST.get
  a <- m
  ST.put (vars, malloc', t)
  return a

local :: MonadMemory m => m a -> m (a, [Text], M.Map Text Int)
local m = do
  (vars, malloc', t) <- ST.get
  a <- m
  (vars', _, t') <- ST.get
  ST.put (vars, malloc', t)
  return (a, vars' L.\\ vars, M.differenceWith (\x y -> Just (x - y)) t' t)

isReference :: T.Type -> Bool
isReference (T.TypeApp (D.Simple "Reference") _) = True
isReference _                                    = False

analyseExpression ::
     MonadMemory m => C.Located C.Expression -> m (C.Located C.Expression)
analyseExpression z@(C.EVariable (D.Simple name) C.:>: pos) = do
  (vars, _, _) <- ST.get
  isReturnStatement <- RWS.ask
  if name `elem` vars && not isReturnStatement
    then do
      ST.modify (\(vars', malloc', t) -> (vars', malloc', t))
      return $
        C.EApplication (C.EVariable (D.Simple "copy_reference") C.:>: pos) [z] C.:>:
        pos
    else return z
analyseExpression (C.ELet (C.Annoted name t) expr body C.:>: pos) = do
  (vars, malloc', t') <- ST.get
  case expr of
    expr'@(C.EApplication (C.EVariable (D.Simple v) C.:>: _) [_] C.:>: _)
      | v `elem` malloc' -> do
        ST.put (name : vars, malloc', M.insert name 1 t')
        body' <-
          local' $
          case body of
            Just body' -> Just <$> analyseExpression body'
            Nothing    -> return Nothing
        expr'' <- analyseExpression expr'
        return $ C.ELet (C.Annoted name t) expr'' body' C.:>: pos
    _ -> do
      when (isReference t) $ ST.put (name : vars, malloc', M.insert name 1 t')
      expr' <- analyseExpression expr
      body' <-
        local' $
        case body of
          Just body' -> Just <$> analyseExpression body'
          Nothing    -> return Nothing
      return $ C.ELet (C.Annoted name t) expr' body' C.:>: pos
analyseExpression (C.EIf e xs ys C.:>: pos) = do
  e' <- analyseExpression e
  xs' <- mapM analyseExpression xs
  ys' <- mapM analyseExpression ys
  return $ C.EIf e' xs' ys' C.:>: pos
analyseExpression (C.EBlock xs C.:>: pos) = do
  let (init', last') = L.splitAt (length xs - 1) xs
  (xs', vars, numbered) <- local $ mapM analyseExpression init'
  let freed =
        [ C.EApplication
          (C.EVariable (D.Simple "free_reference") C.:>: pos)
          [C.EVariable (D.Simple name) C.:>: pos] C.:>:
        pos
        | name <- M.keys numbered
        , name `elem` vars
        , name `notElem`
            getVariableAllocationReturned vars (C.EBlock xs C.:>: pos)
        ]
  lastExpr <- local' $ RWS.local (const True) $ mapM analyseExpression last'
  return $ C.EBlock (xs' ++ freed ++ lastExpr) C.:>: pos
analyseExpression (C.EApplication f xs C.:>: pos) = do
  f' <- analyseExpression f
  xs' <- mapM analyseExpression xs
  return $ C.EApplication f' xs' C.:>: pos
analyseExpression (C.EAssembly name xs C.:>: pos) = do
  xs' <- mapM analyseExpression xs
  return $ C.EAssembly name xs' C.:>: pos
analyseExpression (C.EProperty e name C.:>: pos) = do
  e' <- analyseExpression e
  return $ C.EProperty e' name C.:>: pos
analyseExpression (C.EStruct name xs C.:>: pos) = do
  xs' <-
    mapM (\(C.Annoted name' e) -> C.Annoted name' <$> analyseExpression e) xs
  return $ C.EStruct name xs' C.:>: pos
analyseExpression (C.EList xs C.:>: pos) = do
  xs' <- mapM analyseExpression xs
  return $ C.EList xs' C.:>: pos
analyseExpression (C.EIndex e1 e2 C.:>: pos) = do
  e1' <- analyseExpression e1
  e2' <- analyseExpression e2
  return $ C.EIndex e1' e2' C.:>: pos
analyseExpression (C.EDereference e C.:>: pos) = do
  e' <- analyseExpression e
  return $ C.EDereference e' C.:>: pos
analyseExpression (C.EReference e C.:>: pos) = do
  e' <- analyseExpression e
  return $ C.EReference e' C.:>: pos
analyseExpression z@(C.ESizeOf _ C.:>: _) = do
  return z
analyseExpression (C.EWhile e xs C.:>: pos) = do
  e' <- analyseExpression e
  xs' <- mapM analyseExpression xs
  return $ C.EWhile e' xs' C.:>: pos
analyseExpression (C.EFor name e1 e2 xs C.:>: pos) = do
  e1' <- analyseExpression e1
  e2' <- analyseExpression e2
  xs' <- mapM analyseExpression xs
  return $ C.EFor name e1' e2' xs' C.:>: pos
analyseExpression (C.ELiteral x C.:>: pos) = return $ C.ELiteral x C.:>: pos
analyseExpression (C.EVariable x C.:>: pos) = return $ C.EVariable x C.:>: pos
analyseExpression (C.EUpdate name e C.:>: pos) = do
  e' <- analyseExpression e
  return $ C.EUpdate name e' C.:>: pos
analyseExpression (C.EFunction ret args body C.:>: pos) = do
  body' <- analyseExpression body
  return $ C.EFunction ret args body' C.:>: pos
analyseExpression (C.EAnnotation e t C.:>: pos) = do
  e' <- analyseExpression e
  return $ C.EAnnotation e' t C.:>: pos
analyseExpression (C.Located pos _) = error $ "analyseExpression: " <> show pos
