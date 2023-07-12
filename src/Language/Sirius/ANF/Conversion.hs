{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sirius.ANF.Conversion where

import qualified Language.Sirius.ANF.AST                   as ANF
import           Language.Sirius.ANF.Monad                 (MonadANF, createLet,
                                                            fresh)
import qualified Language.Sirius.CST.Modules.Annoted       as C
import qualified Language.Sirius.Typecheck.Definition.AST  as C
import qualified Language.Sirius.Typecheck.Definition.Type as T
import qualified Data.List as L
import Language.Sirius.ANF.MatchRemover (findPattern, pattern EBinary)
import qualified Language.Sirius.CST.Modules.Literal as C

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
  args' <- mapM convertExpression args
  case call' of
    (ANF.EVariable name , _)-> do
      let args'' = map fst args'
      let stmts = concatMap snd args'
      return (ANF.EApplication name args'', stmts)
    _ -> do
      callName <- fresh
      return
        ( ANF.EApplication callName (map fst args')
        , concatMap snd args' ++ [(C.Annoted callName t, fst call') ])
convertExpression (C.EIf cond th el t) = do
  cond' <- convertExpression cond
  th' <-  mapM convertExpression th
  let th'' = concatMap (\(e, stmts) -> createLet stmts ++ [e]) th'
  el' <- mapM convertExpression el
  let el'' = concatMap (\(e, stmts) -> createLet stmts ++ [e]) el'
  return
    ( ANF.EIfElse (fst cond') th'' el'' t
    , snd cond')
convertExpression (C.ELet n e1 e2 _) = do
  (e1', stmts2) <- convertExpression e1
  case e2 of
    Just e2' -> do
      (e2'', stmts3) <- convertExpression e2'
      return (e2'',  stmts2 ++ [(n, e1')] ++ stmts3)
    Nothing -> return (ANF.ELet n e1', stmts2)
convertExpression (C.EProperty expr field _) = do
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
convertExpression (C.EAssembly (name, _) args) = do
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
convertExpression (C.EInternalField expr field _) = do
  (expr', stmts) <- convertExpression expr
  return (ANF.EInternalField expr' field, stmts)
convertExpression (C.EMatch (expr, ty) cases) = do
  (expr', stmts) <- convertExpression expr
  let decl = ANF.ELet (C.Annoted "$$match$$" ty) expr'
  let var = ANF.EVariable "$$match$$"
  tls <- gets snd
  let cases' = map (\(pat, body, t) -> (findPattern tls pat var, body, t)) cases
  (bodys, stmts') <- unzip <$> mapM (\(_, body, _) -> convertExpression body) cases'
  let cases'' = zip (map (\(e, _, t) -> (e, t)) cases') bodys
  let tys = map (\(_, _, t) -> t) cases'
  let ifs = case cases'' of
          [] -> error "TODO: Match with no cases"
          (x:xs) -> map (\((pats, _), body) -> do
                -- Unwrapping current case to get declarations and conditions
                let (decls, conds) = unzip pats
                let decls' = concatMap (\case
                      Just x' -> [x']
                      Nothing -> []) decls
                let conds' = concatMap (\case
                      Just x' -> [x']
                      Nothing -> []) conds

                if null conds' then
                  \_ -> case body of
                    ANF.EBlock body' -> ANF.EBlock (decls' ++ body')
                    _ -> ANF.EBlock (decls' ++ [body])
                else
                  \e -> ANF.EIfElse (L.foldl1 (EBinary "&&") conds') (case body of
                    ANF.EBlock body' -> decls' ++ body'
                    _ -> decls' ++ [body]) [e] (T.TApp (T.TId "$$Either") tys)
              ) (x:xs)
  let ifs' = L.foldr (\f acc -> f acc) (ANF.EApplication "panic" [ANF.ELiteral (C.String "Unhandled case in pattern matching")]) ifs
  return (ANF.EBlock [decl, ifs'], stmts ++ concat stmts')
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
  modify $ \s -> second (ANF.TStruct name' fields :) s
  let fields' = map (\(C.Annoted name'' t') -> C.Annoted name'' t') fields
  return (ANF.TStruct name' fields')
convertToplevel (C.TExtern _ t) = return (ANF.TExtern t)
convertToplevel (C.TEnumeration _ _) = error "TODO: convertToplevel"
convertToplevel (C.TUnion name fields) = do
  modify $ \s -> second (ANF.TUnion name fields :) s
  return $ ANF.TUnion name fields
convertToplevel _ = error "TODO: convertToplevel"
