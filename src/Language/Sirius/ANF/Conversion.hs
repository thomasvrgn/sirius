{-# LANGUAGE FlexibleContexts #-}

module Language.Sirius.ANF.Conversion where

import qualified Language.Sirius.ANF.AST                   as ANF
import           Language.Sirius.ANF.Monad                 (MonadANF, createLet,
                                                            fresh)
import qualified Language.Sirius.CST.Modules.Annoted       as C
import qualified Language.Sirius.Typecheck.Definition.AST  as C
import qualified Language.Sirius.Typecheck.Definition.Type as T
import Data.Maybe (fromJust)
import qualified Data.List as L

convertExpression ::
     MonadANF m => C.Expression -> m (ANF.Expression, [(C.Annoted T.Type, ANF.Expression)])
convertExpression (C.ELiteral l) = return (ANF.ELiteral l, [])
convertExpression (C.EVariable name' _) = do
  env <- ask
  case L.lookup name' env of
    Just name -> return (ANF.EVariable name, [])
    Nothing   -> return (ANF.EVariable name', [])
convertExpression (C.EApplication call args t) = do
  call' <- convertExpression call
  callName <- fresh
  args' <- mapM convertExpression args
  let name = case call' of
        (ANF.EVariable name', _) -> name'
        _                       -> error "TODO: Application with non-variable"
  return
    ( ANF.EVariable callName
    , snd call' ++ concatMap snd args' ++ [(C.Annoted callName t, ANF.EApplication name (map fst args'))])
convertExpression (C.EIf cond th el) = do
  cond' <- convertExpression cond
  th' <-  mapM convertExpression th
  let th'' = concatMap (\(e, stmts) -> createLet stmts ++ [e]) th'
  el' <- mapM convertExpression el
  let el'' = concatMap (\(e, stmts) -> createLet stmts ++ [e]) el'
  return
    ( ANF.EIf (fst cond') th'' el''
    , snd cond')
convertExpression (C.ELet n e1 e2 t) = do
  (e1', stmts2) <- convertExpression e1
  case e2 of
    Just e2' -> do
      bodyName <- fresh
      (e2'', stmts3) <- local ((C.annotedName n, bodyName):) $ convertExpression e2'
      return (ANF.ELet n e1', stmts2 ++ stmts3 ++ [(C.Annoted bodyName (fromJust t), e2'')])
    Nothing -> return (ANF.ELet n e1', stmts2)
convertExpression (C.EProperty expr field) = do
  (expr', stmts) <- convertExpression expr
  return (ANF.EProperty expr' field, stmts)
convertExpression (C.EStruct n fields) = do
  case n of
    T.TId name -> do
      (fields', exprs, stmts) <-
        unzip3 <$>
        mapM
          (\(C.Annoted name' expr) -> do
             expr' <- convertExpression expr
             return (name', fst expr', snd expr'))
          fields
      return (ANF.EStruct name (zipWith C.Annoted fields' exprs), concat stmts)
    _ -> error "TODO: Struct with type"
convertExpression (C.EList exprs t) = do
  (exprs', stmts) <- unzip <$> mapM convertExpression exprs
  return (ANF.EList t exprs', concat stmts)
convertExpression (C.EIndex expr index) = do
  (expr', stmts) <- convertExpression expr
  (index', stmts') <- convertExpression index
  return (ANF.EIndex expr' index', stmts ++ stmts')
convertExpression (C.EBlock exprs) = do
  exprs' <- mapM convertExpression exprs
  let stmts' = map (\(e, stmts) -> createLet stmts ++ [e]) exprs'
  return (ANF.EBlock (concat stmts'), [])
convertExpression (C.EUpdate update expr) = do
  update' <- convertUpdate update
  (expr', stmts) <- convertExpression expr
  return (ANF.EUpdate update' expr', stmts)
convertExpression (C.EDereference expr) = do
  (expr', stmts) <- convertExpression expr
  return (ANF.EDereference expr', stmts)
convertExpression (C.EReference expr) = do
  (expr', stmts) <- convertExpression expr
  return (ANF.EReference expr', stmts)
convertExpression (C.ESizeOf t) = return (ANF.ESizeOf t, [])
convertExpression (C.EAssembly name args) = do
  (args', stmts) <- unzip <$> mapM convertExpression args
  return (ANF.EAssembly name args', concat stmts)
convertExpression (C.EWhile cond body) = do
  cond' <- convertExpression cond
  body' <- mapM convertExpression body
  let stmts' = map (\(e, stmts) -> createLet stmts ++ [e]) body'
  return (ANF.EWhile (fst cond') (concat stmts'), snd cond')
convertExpression (C.EFor (C.Annoted name _) start end body) = do
  start' <- convertExpression start
  end' <- convertExpression end
  body' <- mapM convertExpression body
  let stmts' = map (\(e, stmts) -> createLet stmts ++ [e]) body'
  return
    ( ANF.EFor name (fst start') (fst end') (concat stmts')
    , snd start' ++ snd end')
convertExpression (C.EDeclaration name ty) = do
  return (ANF.EDeclaration name ty, [])
convertExpression (C.EInternalField expr field) = do
  (expr', stmts) <- convertExpression expr
  return (ANF.EInternalField expr' field, stmts)
convertExpression _ = error "TODO: convertExpression"

convertUpdate :: MonadANF m => C.UpdateExpression -> m ANF.UpdateExpression
convertUpdate (C.UVariable name _) = return (ANF.UVariable name)
convertUpdate (C.UProperty expr field) = do
  expr' <- convertUpdate expr
  return (ANF.UProperty expr' field)
convertUpdate (C.UIndex expr index) = do
  expr' <- convertUpdate expr
  index' <- convertExpression index
  return (ANF.UIndex expr' (fst index'))
convertUpdate (C.UDereference expr) = do
  expr' <- convertUpdate expr
  return (ANF.UDereference expr')
convertUpdate (C.UInternalField expr field) = do
  expr' <- convertUpdate expr
  return (ANF.UInternalField expr' field)

convertToplevel :: MonadANF m => C.Toplevel -> m ANF.Toplevel
convertToplevel (C.TFunction _ (C.Annoted name' t) args body) = do
  (body', stmts) <- convertExpression body
  let body'' =
        case body' of
          ANF.EBlock exprs -> ANF.EBlock (createLet stmts ++ exprs)
          _                -> ANF.EBlock (createLet stmts ++ [body'])
  return (ANF.TFunction (C.Annoted name' t) args body'')
convertToplevel (C.TStruct (C.Annoted name' _) fields) = do
  let fields' = map (\(C.Annoted name'' t') -> C.Annoted name'' t') fields
  return (ANF.TStruct name' fields')
convertToplevel (C.TExtern _ t) = return (ANF.TExtern t)
convertToplevel (C.TEnumeration _ _) = error "TODO: convertToplevel"
convertToplevel (C.TUnion name fields) = return $ ANF.TUnion name fields
convertToplevel _ = error "TODO: convertToplevel"
