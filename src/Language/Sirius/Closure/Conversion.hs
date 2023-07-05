{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module Language.Sirius.Closure.Conversion where

import qualified Control.Monad.RWS                         as RWS
import qualified Data.Bifunctor                            as B
import qualified Data.List                                 as L
import qualified Data.Set                                  as S
import           Language.Sirius.Closure.Free              (Free (free))
import           Language.Sirius.Closure.Monad
import qualified Language.Sirius.CST.Modules.Annoted       as C
import qualified Language.Sirius.Typecheck.Definition.AST  as A
import qualified Language.Sirius.Typecheck.Definition.Type as T
import qualified Control.Monad.State as ST
import qualified Language.Sirius.CST.Modules.Literal as C

substExpr :: A.Expression -> Text -> A.Expression -> A.Expression
substExpr z@(A.EFunction ret args body) name expr =
  if name `elem` map C.annotedName args
    then z
    else A.EFunction ret args (substExpr body name expr)
substExpr z@(A.EVariable name' _) name expr =
  if name == name'
    then expr
    else z
substExpr (A.EApplication e1 e2 ret) name expr =
  A.EApplication (substExpr e1 name expr) (map (\e -> substExpr e name expr) e2) ret
substExpr (A.EStruct n fields) name expr =
  A.EStruct n $
  map (\(C.Annoted n' e) -> C.Annoted n' $ substExpr e name expr) fields
substExpr (A.EProperty e n) name expr = A.EProperty (substExpr e name expr) n
substExpr (A.EIf e1 e2 e3) name expr =
  A.EIf
    (substExpr e1 name expr)
    (substBlock e2 name expr)
    (substBlock e3 name expr)
substExpr z@(A.ELiteral _) _ _ = z
substExpr (A.EList exprs t) name expr =
  A.EList (map (\e -> substExpr e name expr) exprs) t
substExpr (A.EIndex e1 e2) name expr =
  A.EIndex (substExpr e1 name expr) (substExpr e2 name expr)
substExpr (A.EUpdate e1 e2) name expr =
  A.EUpdate (substUpdate e1 name expr) (substExpr e2 name expr)
substExpr (A.EReference e) name expr = A.EReference (substExpr e name expr)
substExpr (A.EDereference e) name expr = A.EDereference (substExpr e name expr)
substExpr (A.ESizeOf e) _ _ = A.ESizeOf e
substExpr (A.EWhile e1 e2) name expr =
  A.EWhile (substExpr e1 name expr) (substBlock e2 name expr)
substExpr (A.EFor n e1 e2 e3) name expr =
  A.EFor
    n
    (substExpr e1 name expr)
    (substExpr e2 name expr)
    (substBlock e3 name expr)
substExpr (A.EClassVariable e n app) _ _ = A.EClassVariable e n app
substExpr (A.EBlock xs) name expr = A.EBlock $ substBlock xs name expr
substExpr _ _ _ = error "substExpr: not an expression"

expr2Update :: A.Expression -> A.UpdateExpression
expr2Update (A.EVariable name t) = A.UVariable name t
expr2Update (A.EIndex e1 e2)     = A.UIndex (expr2Update e1) e2
expr2Update (A.EProperty e n)    = A.UProperty (expr2Update e) n
expr2Update (A.EDereference e)   = A.UDereference (expr2Update e)
expr2Update _                    = error "expr2Update: not an update expression"

substUpdate :: A.UpdateExpression -> Text -> A.Expression -> A.UpdateExpression
substUpdate z@(A.UVariable name' _) name expr =
  if name == name'
    then expr2Update expr
    else z
substUpdate (A.UIndex e1 e2) name expr = A.UIndex (substUpdate e1 name expr) e2
substUpdate (A.UProperty e n) name expr =
  A.UProperty (substUpdate e name expr) n
substUpdate (A.UDereference e) name expr =
  A.UDereference (substUpdate e name expr)
substUpdate (A.UInternalField e n) name expr =
  A.UInternalField (substUpdate e name expr) n

substBlock :: [A.Expression] -> Text -> A.Expression -> [A.Expression]
substBlock (A.ELet n@(C.Annoted name _) e body t:rest) name' expr =
  if name == name'
    then A.ELet n e body t : rest
    else A.ELet
           n
           (substExpr e name' expr)
           (substExpr <$> body <*> pure name' <*> pure expr) t :
         substBlock rest name' expr
substBlock (A.EBlock exprs:rest) name expr =
  A.EBlock (substBlock exprs name expr) : substBlock rest name expr
substBlock (e:rest) name expr =
  substExpr e name expr : substBlock rest name expr
substBlock [] _ _ = []

closureConvert :: MonadClosure m => A.Expression -> Text -> m (A.Expression, [A.Expression], Maybe T.Type)
closureConvert (A.EFunction ret args expr) name = do
  lambdaName <- freshLambda
  envName <- freshEnv
  let envTy = T.TId envName
  let funTy = (envTy : map C.annotedType args) T.:-> ret
  -- traceShowM expr
  let lambdaBody =
        if name == ""
          then expr
          else substExpr expr name (A.EVariable lambdaName funTy)
  -- traceShowM (name, lambdaBody)
  addExcluded lambdaName funTy
  excluded <- gets clExcluded
  let freed = free (A.EFunction ret args lambdaBody)
  let env = S.filter (`S.notMember` excluded) freed
  let declarations =
        L.nub $
        map
          (\n@(C.Annoted f _) ->
             A.ELet n (A.EProperty (A.EVariable "env" envTy) f) Nothing Nothing) $
        S.toList env
  (expr', stmts, ret') <- convertExpression lambdaBody
  -- traceShowM expr'
  let ret'' = case ret' of
        Nothing -> ret
        Just (T.TId n) -> T.TId n
        Just (t T.:-> r) -> t T.:-> r
        Just _ -> ret
  let funTy' = (envTy : map C.annotedType args) T.:-> ret''
  let body = A.EBlock $ declarations ++ stmts ++ [expr']
  RWS.tell
    [ A.TStruct
        (C.Annoted envName [])
        (map (\(C.Annoted n t) -> C.Annoted n t) $ S.toList env)
    , A.TStruct
        (C.Annoted ("env_" <> lambdaName) [])
        [C.Annoted "env" (T.TId envName), C.Annoted "__function__" funTy']
    , A.TFunction
        []
        (C.Annoted lambdaName ret'')
        (C.Annoted "env" envTy : args)
        body
    ]
  let envStruct =
        map (\(C.Annoted n t) -> C.Annoted n (A.EVariable n t)) $ S.toList env
  return (A.EStruct
    (T.TId ("env_" <> lambdaName))
    [ C.Annoted "env" (A.EStruct (T.TId envName) envStruct)
    , C.Annoted "__function__" (A.EVariable lambdaName funTy')
    ], [], Just (T.TId ("env_" <> lambdaName)))
closureConvert _ _ = error "closureConvert: not a function"

doesContainLambda :: A.Expression -> Bool
doesContainLambda (A.EFunction {}) = True
doesContainLambda (A.EBlock xs)       = any doesContainLambda xs
doesContainLambda _                   = False

getFunction :: MonadClosure m => Text -> m (Maybe A.Toplevel)
getFunction name = do
  functions <- gets clToplevels
  return $
    find
      (\case
         A.TFunction _ (C.Annoted name' _) _ _ -> name' == name
         _                                     -> False)
      functions

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

typeOfLit :: C.Literal -> T.Type
typeOfLit (C.Int _) = T.Int
typeOfLit (C.Float _) = T.Float
typeOfLit (C.String _) = T.TString
typeOfLit (C.Bool _) = T.Bool
typeOfLit (C.Char _) = T.Char

convertExpression ::
     MonadClosure m => A.Expression -> m (A.Expression, [A.Expression], Maybe T.Type)
convertExpression (A.EApplication (A.EVariable x t) args ret) = do
  excluded <- isExcluded x t
  tl <- isToplevel x
  (args', stmts, _) <- unzip3 <$> mapM convertExpression args
  (newName, ret', args'') <-
    if any doesContainLambda args
      then do
        found <- getFunction x
        case found of
          Just (A.TFunction _ (C.Annoted name ret') args1 body) -> do
            let args'' =
                  zipWith
                    (\var ty ->
                       (case ty of
                          _ T.:-> _ -> do
                            case var of
                              A.EVariable _ t' -> t'
                              A.EStruct n _ -> n
                              A.EApplication _ _ ret'' -> ret''
                              _ -> error "convertExpression: function not found"
                          x' -> x'))
                    args'
                    (map C.annotedType args1)
            let args3 = zipWith C.Annoted (map C.annotedName args1) args''
            name' <- ((name <> "_") <>) <$> freshLambda
            (body', stmts', ret'') <- convertExpression body
            RWS.tell [A.TFunction [] (C.Annoted name' (fromMaybe ret' ret'')) args3 (A.EBlock (stmts' ++ [body']))]
            return (name', ret'', map C.annotedType args1)
          _ -> error "convertExpression: function not found"
      else do
        found <- getFunction x
        case found of
          Just (A.TFunction gens name args1 body) -> do
            isConverted <- ST.gets (S.member (C.annotedName name) . clConverted)
            if isConverted
              then return (C.annotedName name, Just $ C.annotedType name, map C.annotedType args1)
              else do
                ST.modify $ \s -> s { clConverted = S.insert (C.annotedName name) (clConverted s) }
                (body', stmts', ret') <- convertExpression body
                let name' = case name of
                      C.Annoted name'' _ ->  case ret' of
                        Just ret'' -> C.Annoted name'' ret''
                        Nothing -> name
                RWS.tell [A.TFunction gens name' args1 (A.EBlock (stmts' ++ [body']))]
                return (C.annotedName name, ret', map C.annotedType args1)
          _ -> return (x, Nothing, [])
  let ty = args'' T.:-> fromMaybe ret ret'
  if not excluded && not tl
    then do
      let call =
            A.EApplication
              (A.EProperty (A.EVariable newName ty) "__function__")
              (A.EProperty (A.EVariable newName ty) "env" : args')
              (fromMaybe ret ret')
      return (call, concat stmts, Just ret)
    else
      return (A.EApplication (A.EVariable newName ty) args' ret, concat stmts, ret')
convertExpression (A.EApplication f args ret) = do
  (f', stmts1, r1) <- convertExpression f
  (args', stmts2, _) <- unzip3 <$> mapM convertExpression args
  case r1 of
    Just (T.TId _) -> do
      let call =
            A.EApplication
              (A.EProperty f' "__function__")
              (A.EProperty f' "env" : args')
              ret
      return (call, stmts1 ++ concat stmts2, r1)
    Just (_ T.:-> ret') -> do
      let call = A.EApplication f' args' ret'
      return (call, stmts1 ++ concat stmts2, r1)
    _ ->  do
      let call = A.EApplication f' args' ret
      return (call, stmts1 ++ concat stmts2, r1)
convertExpression (A.EIf cond t f) = do
  ((calls1, ret1), (calls2, ret2)) <- case (viaNonEmpty last t, viaNonEmpty last f) of
    (Just z1@A.EFunction {}, Just z2@A.EFunction {}) -> do
      let t' = viaNonEmpty init t
      let f' = viaNonEmpty init f
      case (t', f') of
        (Just t'', Just f'') -> return ((t'', Just z1), (f'', Just z2))
        _ -> return ((t, Nothing), (f, Nothing))
    _ -> return ((t, Nothing), (f, Nothing))
  (cond', stmts1, _) <- convertExpression cond
  (t', stmts2, _) <- unzip3 <$> mapM convertExpression calls1
  (f', stmts3, _) <- unzip3 <$> mapM convertExpression calls2

  case (ret1, ret2) of
    (Just (A.EFunction ty args expr), Just (A.EFunction ty' args' expr'))
      | ty == ty' && length args == length args' -> do
        let fun = A.EFunction ty args (A.EIf cond' [expr] [expr'])
        (retT, stmts4, r2') <- convertExpression fun

        let stmts' = stmts1 ++ concat stmts2 ++ concat stmts3 ++ [A.EIf cond' t' f'] ++ stmts4 

        return (retT, stmts', r2')
    _ -> return (A.EIf cond' t' f', stmts1 ++ concat stmts2 ++ concat stmts3, Nothing)
convertExpression (A.ELiteral x) = return (A.ELiteral x, [], Just $ typeOfLit x)
convertExpression (A.EList xs t) = do
  (xs', stmts, _) <- unzip3 <$> mapM convertExpression xs
  return (A.EList xs' t, concat stmts, Just $ T.TList t)
convertExpression z@A.EFunction {} = closureConvert z ""
convertExpression (A.EIndex arr idx) = do
  (arr', stmts1, _) <- convertExpression arr
  (idx', stmts2, _) <- convertExpression idx
  return (A.EIndex arr' idx', stmts1 ++ stmts2, Nothing)
convertExpression (A.EProperty str name) = do
  (str', stmts, _) <- convertExpression str
  return (A.EProperty str' name, stmts, Nothing)
convertExpression (A.EStruct n xs) = do
  (xs', stmts) <-
    B.second unzip3 . unzip <$>
    mapM (\(C.Annoted n' e) -> (n', ) <$> convertExpression e) xs
  return (A.EStruct n (zipWith C.Annoted xs' $ fst3 stmts), concat $ snd3 stmts, Just n)
convertExpression (A.EVariable x t) = do
  found <- getFunction x
  case found of
    Just (A.TFunction gens (C.Annoted name ret) args body) -> do
      isConverted <- ST.gets (S.member x . clConverted)
      if isConverted
        then return (A.EVariable x ret, [], Just ret)
        else do
          ST.modify $ \s -> s { clConverted = S.insert x (clConverted s) }
          (body', stmts, r) <- convertExpression body
          RWS.tell [A.TFunction gens (C.Annoted name (fromMaybe ret r)) args (A.EBlock (stmts ++ [body']))]
          return (A.EVariable x t, [], Just (fromMaybe ret r))
    _ -> return (A.EVariable x t, [], Just t)
convertExpression (A.EUpdate updated e) = do
  (e', stmts, _) <- convertExpression e
  return (A.EUpdate updated e', stmts, Nothing)
convertExpression (A.ELet (C.Annoted name t) e body ty') = do
  (e', stmts1, valueTy) <- convertExpression e
  (body', stmts2, ty) <-
    case body of
      Nothing -> return (Nothing, [], Nothing)
      Just body' -> do
        (body'', stmts2, ty) <- convertExpression body'
        return (Just body'', stmts2, ty)
  return (A.ELet (C.Annoted name (fromMaybe t valueTy)) e' body' ty', stmts1 ++ stmts2, ty)
convertExpression (A.EDereference e) = do
  (e', stmts, ty) <- convertExpression e
  return (A.EDereference e', stmts, case ty of
    Just (T.TAddr t) -> Just t
    _ -> Nothing)
convertExpression (A.EBlock exprs) = do
  (exprs', stmts, tys) <- unzip3 <$> mapM convertExpression exprs
  let ty = viaNonEmpty last tys
  return (A.EBlock exprs', concat stmts, case ty of
    Just (Just t) -> Just t
    _ -> Nothing)
convertExpression (A.EReference e) = do
  (e', stmts, ty) <- convertExpression e
  return (A.EReference e', stmts, T.TAddr <$> ty)
convertExpression (A.EWhile cond body) = do
  (cond', stmts1, _) <- convertExpression cond
  (body', stmts2, tys) <- unzip3 <$> mapM convertExpression body
  let ty = viaNonEmpty last tys
  return (A.EWhile cond' body', stmts1 ++ concat stmts2, case ty of
    Just (Just t) -> Just t
    _ -> Nothing)
convertExpression (A.EFor n from to body) = do
  (from', stmts1, _) <- convertExpression from
  (to', stmts2, _) <- convertExpression to
  (body', stmts3, tys) <- unzip3 <$> mapM convertExpression body
  let ty = viaNonEmpty last tys
  return (A.EFor n from' to' body', stmts1 ++ stmts2 ++ concat stmts3, case ty of
    Just (Just t) -> Just t
    _ -> Nothing)
convertExpression (A.ESizeOf e) = return (A.ESizeOf e, [], Just T.Int)
convertExpression (A.EClassVariable n t app) =
  return (A.EClassVariable n t app, [], Nothing)
convertExpression (A.ELocated e _) = convertExpression e
convertExpression (A.EAssembly op args) = do
  (args', stmts, _) <- unzip3 <$> mapM convertExpression args
  return (A.EAssembly op args', concat stmts, Nothing)
convertExpression (A.EDeclaration n t) = do
  return (A.EDeclaration n t, [], Nothing)
convertExpression (A.EInternalField expr t) = do
  (expr', stmts, _) <- convertExpression expr
  return (A.EInternalField expr' t, stmts, Nothing)

convertToplevel :: MonadClosure m => A.Toplevel -> m ()
convertToplevel (A.TFunction n ret args body) = do
  addExcluded
    (C.annotedName ret)
    (map C.annotedType args T.:-> C.annotedType ret)
  when (C.annotedName ret == "main") $ do
    (body', _, _) <- convertExpression body
    RWS.tell [A.TFunction n ret args body']
convertToplevel (A.TStruct n fields) = RWS.tell [A.TStruct n fields]
convertToplevel (A.TExtern gens (C.Annoted name ty)) = do
  addExcluded name ty
  RWS.tell [A.TExtern gens (C.Annoted name ty)]
convertToplevel (A.TEnumeration n fields) = do
  mapM_ (\(C.Annoted name ty) -> addExcluded name ty) fields
  RWS.tell [A.TEnumeration n fields]
convertToplevel (A.TUnion n fields) = RWS.tell [A.TUnion n fields]
convertToplevel A.TFunctionProp {} =
  error "Should not encounter function properties during closure conversion."
