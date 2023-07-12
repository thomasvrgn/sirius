{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module Language.Sirius.Closure.Conversion where

import qualified Control.Monad.RWS                         as RWS
import qualified Control.Monad.State                       as ST
import qualified Data.Bifunctor                            as B
import qualified Data.List                                 as L
import           Data.Maybe                                (fromJust)
import qualified Data.Set                                  as S
import qualified Data.Text                                 as T
import           Language.Sirius.Closure.Free              (Free (free))
import           Language.Sirius.Closure.Monad
import qualified Language.Sirius.CST.Modules.Annoted       as C
import qualified Language.Sirius.CST.Modules.Literal       as C
import qualified Language.Sirius.Typecheck.Definition.AST  as A
import qualified Language.Sirius.Typecheck.Definition.Type as T

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
  A.EApplication
    (substExpr e1 name expr)
    (map (\e -> substExpr e name expr) e2)
    ret
substExpr (A.EStruct n fields) name expr =
  A.EStruct n $
  map (\(C.Annoted n' e) -> C.Annoted n' $ substExpr e name expr) fields
substExpr (A.EProperty e n r) name expr =
  A.EProperty (substExpr e name expr) n r
substExpr (A.EIf e1 e2 e3 t) name expr =
  A.EIf
    (substExpr e1 name expr)
    (substBlock e2 name expr)
    (substBlock e3 name expr)
    t
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
expr2Update (A.EProperty e n _)  = A.UProperty (expr2Update e) n
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
           (substExpr <$> body <*> pure name' <*> pure expr)
           t :
         substBlock rest name' expr
substBlock (A.EBlock exprs:rest) name expr =
  A.EBlock (substBlock exprs name expr) : substBlock rest name expr
substBlock (e:rest) name expr =
  substExpr e name expr : substBlock rest name expr
substBlock [] _ _ = []

closureConvert ::
     MonadClosure m
  => A.Expression
  -> Text
  -> m (A.Expression, [A.Expression], Maybe T.Type)
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
          (\n@(C.Annoted f t) ->
             A.ELet
               n
               (A.EProperty (A.EVariable "env" envTy) f (Just t))
               Nothing
               Nothing) $
        S.toList env
  (expr', stmts, ret') <- convertExpression lambdaBody
  -- traceShowM expr'
  let ret'' =
        case ret' of
          Nothing          -> ret
          Just (T.TId n)   -> T.TId n
          Just (t T.:-> r) -> t T.:-> r
          Just _           -> ret
  let funTy' = (envTy : map C.annotedType args) T.:-> ret''
  let body = A.EBlock $ declarations ++ stmts ++ [expr']
  RWS.tell
    [ A.TStruct
        (C.Annoted envName [])
        (map (\(C.Annoted n t) -> C.Annoted n t) $ S.toList env)
    , A.TStruct
        (C.Annoted ("env_" <> lambdaName) [])
        [C.Annoted "__function__" funTy', C.Annoted "env" (T.TId envName)]
    , A.TFunction
        []
        (C.Annoted lambdaName ret'')
        (C.Annoted "env" envTy : args)
        body
    ]
  ST.modify $ \s ->
    s
      { clStructsNames = S.insert ("env_" <> lambdaName) (clStructsNames s)
      , clStructsFuns =
          S.insert ("env_" <> lambdaName, funTy') (clStructsFuns s)
      }
  let envStruct =
        map (\(C.Annoted n t) -> C.Annoted n (A.EVariable n t)) $ S.toList env
  return
    ( A.EStruct
        (T.TId ("env_" <> lambdaName))
        [ C.Annoted "env" (A.EStruct (T.TId envName) envStruct)
        , C.Annoted "__function__" (A.EVariable lambdaName funTy')
        ]
    , []
    , Just (T.TId ("env_" <> lambdaName)))
closureConvert _ _ = error "closureConvert: not a function"

doesContainLambda :: A.Expression -> Bool
doesContainLambda A.EFunction {} = True
doesContainLambda (A.EIf e1 e2 e3 t) =
  any doesContainLambda (e1 : e2 ++ e3) || isLambdaType t
doesContainLambda (A.EWhile e1 e2) = any doesContainLambda (e1 : e2)
doesContainLambda (A.EBlock xs) = any doesContainLambda xs
doesContainLambda (A.EVariable _ t) = isLambdaType t
doesContainLambda _ = False

isLambdaType :: T.Type -> Bool
isLambdaType (T.TId n)
  | "env_$$l" `T.isPrefixOf` n = True
isLambdaType (T.TId n)
  | "env_" `T.isPrefixOf` n = False
isLambdaType (T.TId n)
  | "$$l" `T.isPrefixOf` n = True
isLambdaType (T.TApp (T.TId "$$Either") tys) = all isLambdaType tys
isLambdaType (_ T.:-> _) = True
isLambdaType _ = False

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
typeOfLit (C.Int _)    = T.Int
typeOfLit (C.Float _)  = T.Float
typeOfLit (C.String _) = T.TString
typeOfLit (C.Bool _)   = T.Bool
typeOfLit (C.Char _)   = T.Char

doesReturnLambda :: A.Expression -> Bool
doesReturnLambda A.EFunction {} = True
doesReturnLambda (A.EBlock xs) =
  fromJust $ viaNonEmpty last xs <&> doesReturnLambda
doesReturnLambda (A.EIf _ e1 e2 _) =
  fromJust (viaNonEmpty last e1 <&> doesReturnLambda) ||
  fromJust (viaNonEmpty last e2 <&> doesReturnLambda)
doesReturnLambda (A.EWhile _ e) =
  fromJust (viaNonEmpty last e <&> doesReturnLambda)
doesReturnLambda (A.EFor _ _ _ e) =
  fromJust (viaNonEmpty last e <&> doesReturnLambda)
doesReturnLambda _ = False

convertExpression ::
     MonadClosure m
  => A.Expression
  -> m (A.Expression, [A.Expression], Maybe T.Type)
convertExpression (A.EApplication (A.EVariable x t) args ret) = do
  excluded <- isExcluded x t
  tl <- isToplevel x
  (args', stmts, tys) <- unzip3 <$> mapM convertExpression args
  (newName, ret', args'', closureRet) <-
    do found <- getFunction x
       case found of
         Just (A.TFunction gens name args1 body) -> do
           isConverted <- ST.gets (S.member (C.annotedName name) . clConverted)
           if isConverted
             then return
                    ( C.annotedName name
                    , Just $ C.annotedType name
                    , if all isJust tys
                        then catMaybes tys
                        else map C.annotedType args1
                    , case C.annotedType name of
                        _ T.:-> ret'' -> Just ret''
                        _ -> Nothing)
             else do
               ST.modify $ \s ->
                 s {clConverted = S.insert (C.annotedName name) (clConverted s)}
               (body', stmts', ret') <- convertExpression body
               let name' =
                     case name of
                       C.Annoted name'' _ ->
                         case ret' of
                           Just ret'' -> C.Annoted name'' ret''
                           Nothing    -> name
               let args2 =
                     zipWith
                       C.Annoted
                       (map C.annotedName args1)
                       (if all isJust tys
                          then catMaybes tys
                          else map C.annotedType args1)
               RWS.tell
                 [A.TFunction gens name' args2 (A.EBlock (stmts' ++ [body']))]
               return
                 ( C.annotedName name
                 , ret'
                 , if all isJust tys
                     then catMaybes tys
                     else map C.annotedType args2
                 , case C.annotedType name of
                    _ T.:-> t' -> Just t'
                    _          -> Nothing)
         _ -> do
           (A.EVariable x' _, _, Just (_ T.:-> t')) <-
             convertExpression (A.EVariable x t)
           return (x', Just t', catMaybes tys, Just t')
  let ty =
        case ret' of
          Just t' ->
            case args'' of
              [] -> t'
              _  -> args'' T.:-> t'
          Nothing -> ret
  if not excluded && not tl
    then do
      let call =
            A.EApplication
              (A.EProperty (A.EVariable newName ty) "__function__" Nothing)
              (A.EProperty (A.EVariable newName ty) "env" Nothing : args')
              ty
      return (call, concat stmts, closureRet <|> ret')
    else return
           ( A.EApplication (A.EVariable newName ty) args' ty
           , concat stmts
           , ret')
convertExpression (A.EApplication f args ret) = do
  (f', stmts1, r1) <- convertExpression f
  (args', stmts2, _) <- unzip3 <$> mapM convertExpression args
  structs <- ST.gets clStructsNames
  fnStructs <- ST.gets clStructsFuns
  name <- freshLambda
  case r1 of
    Just z@(T.TId n)
      | n `S.member` structs -> do
        let ty = L.lookup n (S.toList fnStructs)
        let call =
              A.ELet
                (C.Annoted name z)
                f'
                (Just $
                 A.EApplication
                   (A.EProperty (A.EVariable name z) "__function__" Nothing)
                   (A.EProperty (A.EVariable name z) "env" Nothing : args')
                   (fromJust ty))
                ty
        return (call, stmts1 ++ concat stmts2, case ty of Just (_ T.:-> t) -> Just t; _ -> Nothing)
    Just z@(T.TApp (T.TId n) tys)
      | all (\(T.TId n') -> n' `S.member` structs) tys && "$$Either" `T.isPrefixOf` n -> do
        let ns = map (\(T.TId n') -> n') tys
        let ty' = map (`L.lookup` S.toList fnStructs) ns
        let tys' = catMaybes ty'
        let ty = case tys' of
              [t] -> Just t
              xs -> Just $ T.TApp (T.TId n) xs
        let env = T.TApp (T.TId n) $ mapMaybe (\case
                (envTy:_) T.:-> _ -> Just envTy
                _ -> Nothing) tys'
        let call =
              A.ELet
                (C.Annoted name z)
                f'
                (Just $
                 A.EApplication
                   (A.EProperty (A.EVariable name z) "__function__" Nothing)
                   (A.EProperty (A.EVariable name z) "env" (Just env) : args')
                   (fromJust ty))
                ty
        return (call, stmts1 ++ concat stmts2, case ty of Just (_ T.:-> t) -> Just t; _ -> Nothing)
    Just (_ T.:-> ret') -> do
      let call = A.EApplication f' args' ret'
      return (call, stmts1 ++ concat stmts2, Just ret')
    _ -> do
      let call = A.EApplication f' args' ret
      return (call, stmts1 ++ concat stmts2, Just ret)
convertExpression (A.EIf cond t f _) = do
  (cond', stmts1, _) <- convertExpression cond
  (t', stmts2, t1) <- unzip3 <$> mapM convertExpression t
  (f', stmts3, t2) <- unzip3 <$> mapM convertExpression f
  t'' <-
    case (viaNonEmpty last t1, viaNonEmpty last t2) of
      (Just (Just t1'), Just (Just t2')) -> return $ Just (t1', t2')
      _                                  -> return Nothing
  l <- freshLambda
  let ty' =
        case t'' of
          Just (t1', t2') -> Just (T.TApp (T.TId ("$$Either" <> l)) [t1', t2'])
          Nothing         -> if t1 == t2 then fromJust (viaNonEmpty last t1) else Nothing
  return
    ( A.EIf cond' t' f' (fromJust ty')
    , stmts1 ++ concat stmts2 ++ concat stmts3
    , ty')
convertExpression (A.EMatch (expr, ty) cases) = do
  (expr', stmts1, _) <- convertExpression expr
  (cases', stmts2, tys) <- unzip3 <$> mapM convertCase cases
  l <- freshLambda
  return
    ( A.EMatch (expr', ty) cases'
    , stmts1 ++ concat stmts2
    , if all (isLambdaType . fromJust) tys then Just (T.TApp (T.TId ("$$Either" <> l)) (catMaybes tys)) else fromJust (viaNonEmpty head tys))
  where
    convertCase (pat, expr', t) = do
      (expr'', stmts2, ty') <- convertExpression expr'
      return ((pat, expr'', fromMaybe t ty'), stmts2, ty')
convertExpression (A.ELiteral x) = return (A.ELiteral x, [], Just $ typeOfLit x)
convertExpression (A.EList xs t) = do
  (xs', stmts, _) <- unzip3 <$> mapM convertExpression xs
  return (A.EList xs' t, concat stmts, Just $ T.TList t)
convertExpression z@A.EFunction {} = closureConvert z ""
convertExpression (A.EIndex arr idx) = do
  (arr', stmts1, t) <- convertExpression arr
  (idx', stmts2, _) <- convertExpression idx
  case t of
    Just (T.TList t') -> return (A.EIndex arr' idx', stmts1 ++ stmts2, Just t')
    _                 -> return (A.EIndex arr' idx', stmts1 ++ stmts2, Nothing)
convertExpression (A.EProperty str name t) = do
  (str', stmts, _) <- convertExpression str
  return (A.EProperty str' name t, stmts, t)
convertExpression (A.EStruct n xs) = do
  (xs', stmts) <-
    B.second unzip3 . unzip <$>
    mapM (\(C.Annoted n' e) -> (n', ) <$> convertExpression e) xs
  return
    ( A.EStruct n (zipWith C.Annoted xs' $ fst3 stmts)
    , concat $ snd3 stmts
    , Just n)
convertExpression (A.EVariable x t) = do
  found <- getFunction x
  case found of
    Just (A.TFunction gens (C.Annoted name ret) args body) -> do
      isConverted <- ST.gets (S.member x . clConverted)
      if isConverted
        then return (A.EVariable x ret, [], Just ret)
        else do
          ST.modify $ \s -> s {clConverted = S.insert x (clConverted s)}
          (body', stmts, r) <- convertExpression body
          RWS.tell
            [ A.TFunction
                gens
                (C.Annoted name (fromMaybe ret r))
                args
                (A.EBlock (stmts ++ [body']))
            ]
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
  return
    ( A.ELet (C.Annoted name (fromMaybe t valueTy)) e' body' ty'
    , stmts1 ++ stmts2
    , ty)
convertExpression (A.EDereference e) = do
  (e', stmts, ty) <- convertExpression e
  return
    ( A.EDereference e'
    , stmts
    , case ty of
        Just (T.TAddr t) -> Just t
        _                -> Nothing)
convertExpression (A.EBlock exprs) = do
  (exprs', stmts, tys) <- unzip3 <$> mapM convertExpression exprs
  let ty = viaNonEmpty last tys
  return
    ( A.EBlock exprs'
    , concat stmts
    , case ty of
        Just (Just t) -> Just t
        _             -> Just T.Void)
convertExpression (A.EReference e) = do
  (e', stmts, ty) <- convertExpression e
  return (A.EReference e', stmts, T.TAddr <$> ty)
convertExpression (A.EWhile cond body) = do
  (cond', stmts1, _) <- convertExpression cond
  (body', stmts2, tys) <- unzip3 <$> mapM convertExpression body
  let ty = viaNonEmpty last tys
  return
    ( A.EWhile cond' body'
    , stmts1 ++ concat stmts2
    , case ty of
        Just (Just t) -> Just t
        _             -> Just T.Void)
convertExpression (A.EFor n from to body) = do
  (from', stmts1, _) <- convertExpression from
  (to', stmts2, _) <- convertExpression to
  (body', stmts3, tys) <- unzip3 <$> mapM convertExpression body
  let ty = viaNonEmpty last tys
  return
    ( A.EFor n from' to' body'
    , stmts1 ++ stmts2 ++ concat stmts3
    , case ty of
        Just (Just t) -> Just t
        _             -> Just T.Void)
convertExpression (A.ESizeOf e) = return (A.ESizeOf e, [], Just T.Int)
convertExpression (A.EClassVariable n t app) =
  return (A.EClassVariable n t app, [], Nothing)
convertExpression (A.ELocated e _) = convertExpression e
convertExpression (A.EAssembly op@(_, t) args) = do
  (args', stmts, _) <- unzip3 <$> mapM convertExpression args
  return (A.EAssembly op args', concat stmts, Just t)
convertExpression (A.EDeclaration n t) =
  return (A.EDeclaration n t, [], Just T.Void)
convertExpression (A.EInternalField expr t ty) = do
  (expr', stmts, _) <- convertExpression expr
  return (A.EInternalField expr' t ty, stmts, ty)

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
  mapM_
    (\(C.Annoted name ty) ->
       case ty of
         _ T.:-> _ -> addExcluded name ty
         _         -> addExcluded name ([] T.:-> ty))
    fields
  RWS.tell [A.TEnumeration n fields]
convertToplevel (A.TUnion n fields) = RWS.tell [A.TUnion n fields]
convertToplevel A.TFunctionProp {} =
  error "Should not encounter function properties during closure conversion."
