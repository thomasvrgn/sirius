{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}

module Language.Sirius.Typecheck.Checker where

import qualified Language.Sirius.Typecheck.Definition.AST       as A
import           Language.Sirius.Typecheck.Definition.Monad
import qualified Language.Sirius.Typecheck.Definition.Type      as T
import qualified Language.Sirius.Typecheck.Modules.Parser       as P

import qualified Control.Monad.Except                           as E
import qualified Control.Monad.State                            as ST
import qualified Data.Bifunctor                                 as BF
import qualified Data.List                                      as L
import qualified Data.Map                                       as M
import qualified Data.Set                                       as S
import qualified Language.Sirius.CST.Expression                 as C
import qualified Language.Sirius.CST.Modules.Annoted            as C
import qualified Language.Sirius.CST.Modules.Literal            as C
import qualified Language.Sirius.CST.Modules.Located            as C
import qualified Language.Sirius.CST.Modules.Namespaced         as D
import qualified Language.Sirius.Typecheck.ConstraintSolver     as CS
import qualified Language.Sirius.Typecheck.Definition.Monad     as M
import qualified Language.Sirius.Typecheck.Modules.Apply        as AP
import qualified Language.Sirius.Typecheck.Modules.Substitution as T
import           Prelude                                        hiding
                                                                (Constraint,
                                                                 Type)

type Infer f m f'
   = MonadChecker m =>
       f -> m (T.Type, f')

generalize :: Envs -> T.Type -> T.Scheme
generalize env t = T.Forall vars t
  where
    vars = S.toList (T.free t S.\\ T.free env)

unify :: M.MonadChecker m => AP.Constraint -> m ()
unify c = ST.modify (\s -> s {M.constraints = M.constraints s ++ [c]})

withClass ::
     M.MonadChecker m => (T.Type, Text, Bool) -> T.Scheme -> m a -> C.Position -> m a
withClass z@(ty, name, b) scheme m pos = do
  env <- ST.gets M.classes
  case M.lookup z env of
    Nothing -> do
      ST.modify (\s -> s {M.classes = M.insert (ty, name, b) scheme env})
      a <- m
      ST.modify (\s -> s {M.classes = M.delete (ty, name, b) (M.classes s)})
      return a
    Just _ -> E.throwError ("Class " <> name <> " already exists", Nothing, pos)

withVariable :: M.MonadChecker m => (Text, T.Scheme) -> m a -> m a
withVariable (name, scheme) =
  M.local
    (BF.first $ \env -> do
       let env' = M.delete name env
       M.insert name scheme env')

withVariables :: M.MonadChecker m => [(Text, T.Scheme)] -> m a -> m a
withVariables xs =
  M.local
    (BF.first $ \env -> do
       let env' = foldr (M.delete . fst) env xs
       foldr (uncurry M.insert) env' xs)

withReturnType :: M.MonadChecker m => T.Type -> m a -> m a
withReturnType t m = do
  ret <- ST.gets M.returnType
  ST.modify (\s -> s {M.returnType = t})
  a <- m
  ST.modify (\s -> s {M.returnType = ret})
  return a

findName :: T.Type -> Text
findName (T.TApp t _) = findName t
findName (T.TId name) = name
findName _            = error "Invalid type"

inferExpression :: M.Infer (C.Located C.Expression) m A.Expression
inferExpression (C.Located pos C.EHole) = do
  tv <- M.fresh
  unify (AP.Hole tv, pos)
  return (tv, A.EVariable "_" tv)
inferExpression (C.Located pos (C.EVariable (D.Simple name))) = do
  env <- ST.gets M.variables
  case M.lookup name env of
    Nothing -> do
      types' <- ST.gets M.types
      case M.lookup name types' of
        Nothing ->
          E.throwError ("Variable " <> name <> " not found", Nothing, pos)
        Just scheme -> do
          t <- M.instantiate scheme
          case t of
            T.TRec _ fields -> do
              let args = map snd fields
              return
                (args T.:-> t, A.ELocated (A.EVariable name (args T.:-> t)) pos)
            _ T.:-> _ -> return (t, A.ELocated (A.EVariable name t) pos)
            _ -> return (t, A.EApplication (A.ELocated (A.EVariable name ([] T.:-> t)) pos) [] t)
    Just scheme -> do
      t <- M.instantiate scheme
      return (t, A.ELocated (A.EVariable name t) pos)
inferExpression (C.EApplication (C.EProperty z field C.:>: p2) args C.:>: _) = do
  (t, z') <- M.local' $ inferExpression z
  (ts, args') <- unzip <$> mapM (M.local' . inferExpression) args
  ret <- M.fresh
  unify (AP.Class field t (ts T.:-> ret), p2)
  return
    ( ret
    , A.EApplication
        (A.ELocated (A.EClassVariable field t (ts T.:-> ret)) p2)
        (z' : args')
        ((t:ts) T.:-> ret))
inferExpression (C.Located pos (C.EApplication f xs)) = do
  (t, f') <- M.local' $ inferExpression f
  (ts, xs') <- unzip <$> mapM (M.local' . inferExpression) xs
  ret <- M.fresh
  unify (t AP.:~: (ts T.:-> ret), pos)
  return (ret, A.EApplication f' xs' (ts T.:-> ret))
inferExpression (C.Located pos (C.ELet (C.Annoted name ty) expr body)) = do
  generics' <- ST.gets M.generics
  tv <- P.toWithEnv ty generics'
  (t', e') <- withVariable (name, T.Forall [] tv) $ inferExpression expr
  unify (tv AP.:~: t', pos)
  let scheme = T.Forall [] tv
  ST.modify $ \s' -> s' {M.variables = M.insert name scheme (M.variables s')}
  (t'', e'') <-
    case body of
      Just body' -> do
        (t'', body'') <- M.local' $ inferExpression body'
        return (t'', Just body'')
      Nothing -> return (T.Void, Nothing)
  return (t'', A.ELet (name C.:@ tv) e' e'' $ Just t'')
inferExpression (C.Located pos (C.EIf cond then' else')) = do
  (t, cond') <- M.local' $ inferExpression cond
  unify (t AP.:~: T.Bool, pos)
  (t', then'') <- unzip <$> M.local' (mapM inferExpression then')
  (t'', else'') <- unzip <$> M.local' (mapM inferExpression else')
  unify (createType t' AP.:~: createType t'', pos)
  return (createType t', A.EIf cond' then'' else'' (createType t'))
inferExpression (C.Located _ (C.ELiteral l)) = do
  (t, l') <- inferLiteral l
  return (t, A.ELiteral l')
inferExpression (C.Located pos (C.EFunction ret args body)) = do
  generics' <- ST.gets M.generics
  ret' <- P.toWithEnv ret generics'
  args' <- mapM ((`P.toWithEnv` generics') . C.annotedType) args
  let args'' =
        zipWith (\n t -> (n, T.Forall [] t)) (map C.annotedName args) args'
  (t, body') <-
    withVariables args'' $ withReturnType ret' $ inferExpression body
  unify (t AP.:~: ret', pos)
  return
    ( args' T.:-> t
    , A.EFunction ret' (zipWith C.Annoted (map C.annotedName args) args') body')
inferExpression (C.Located pos (C.EProperty expr name)) = do
  tv <- M.fresh
  (t, e') <- M.local' $ inferExpression expr
  unify (AP.Field name tv t, pos)
  return (tv, A.EProperty e' name (Just tv))
inferExpression (C.Located pos (C.EStruct ty fields)) = do
  structs <- ST.gets M.types
  gens <- ST.gets M.generics
  ty' <- P.toWithEnv ty gens
  let name = findName ty'
  case M.lookup name structs of
    Just (T.Forall _ (T.TRec _ fields')) -> do
      (tys, fields'') <-
        unzip <$>
        mapM
          (\(C.Annoted name' expr) -> do
             case L.lookup name' fields' of
               Just t -> do
                 (t', expr') <- M.local' $ inferExpression expr
                 unify (t AP.:~: t', pos)
                 return (t', expr')
               Nothing ->
                 E.throwError ("Field not in struct: " <> name, Nothing, pos))
          fields
      let typedFields = zip (map C.annotedName fields) tys
      return
        ( T.TRec name typedFields
        , A.EStruct ty' (zipWith C.Annoted (map C.annotedName fields) fields''))
    Just _ -> E.throwError ("Misformed struct: " <> name, Nothing, pos)
    Nothing -> E.throwError ("Struct not in scope: " <> name, Nothing, pos)
inferExpression (C.Located pos (C.EList exprs)) = do
  tv <- M.fresh
  (tys, exprs') <- unzip <$> mapM (M.local' . inferExpression) exprs
  mapM_ (\t -> unify (t AP.:~: tv, pos)) tys
  return (T.TAddr tv, A.EList exprs' tv)
inferExpression (C.Located pos (C.EIndex expr index)) = do
  tv <- M.fresh
  (t, expr') <- M.local' $ inferExpression expr
  unify (t AP.:~: T.TAddr tv, pos)
  (t', index') <- M.local' $ inferExpression index
  unify (t' AP.:~: T.Int, pos)
  return (tv, A.EIndex expr' index')
inferExpression (C.Located _ (C.EBlock exprs)) = do
  (tys, exprs') <- unzip <$> M.local' (mapM inferExpression exprs)
  return (createType tys, A.EBlock exprs')
inferExpression (C.Located pos (C.EUpdate u e)) = do
  (t, u') <- M.local' $ inferUpdate u
  (t', e') <- M.local' $ inferExpression e
  unify (t AP.:~: t', pos)
  return (T.Void, A.EUpdate u' e')
inferExpression (C.Located pos (C.EDereference e)) = do
  tv <- M.fresh
  (t, e') <- M.local' $ inferExpression e
  unify (t AP.:~: T.TAddr tv, pos)
  return (tv, A.EDereference e')
inferExpression (C.Located _ (C.EReference e)) = do
  (t, e') <- M.local' $ inferExpression e
  return (T.TAddr t, A.EReference e')
inferExpression (C.Located pos (C.EFor name from to body)) = do
  (t, expr') <- M.local' $ inferExpression from
  unify (t AP.:~: T.Int, pos)
  (t', expr'') <- M.local' $ inferExpression to
  unify (t' AP.:~: T.Int, pos)
  (t'', body') <-
    unzip <$>
    withVariables [(name, T.Forall [] T.Int)] (mapM inferExpression body)
  return (createType t'', A.EFor (C.Annoted name T.Int) expr' expr'' body')
inferExpression (C.Located pos (C.EWhile cond body)) = do
  (t, cond') <- M.local' $ inferExpression cond
  unify (t AP.:~: T.Bool, pos)
  (t', body') <- unzip <$> M.local' (mapM inferExpression body)
  unify (createType t' AP.:~: T.Void, pos)
  return (T.Void, A.EWhile cond' body')
inferExpression (C.Located _ (C.ESizeOf t)) = do
  gens <- ST.gets M.generics
  t' <- P.toWithEnv t gens
  return (T.Int, A.ESizeOf t')
inferExpression (C.Located pos (C.EAnnotation e t)) = do
  gens <- ST.gets M.generics
  t' <- P.toWithEnv t gens
  (t'', e') <- M.local' $ inferExpression e
  unify (t' AP.:~: t'', pos)
  return (t', e')
inferExpression (C.Located _ (C.EAssembly op args)) = do
  t <- M.fresh
  args' <- mapM (M.local' . inferExpression) args
  let (_, args'') = unzip args'
  return (t, A.EAssembly (op, t) args'')
inferExpression (C.Located pos (C.EMatch expr cases)) = do
  (t, e') <- local' $ inferExpression expr
  (tys, cases') <- L.unzip <$> local' (forM cases (\(pat, expr') -> do
    (t', pat', vars) <- inferPattern pat
    unify (t AP.:~: t', pos)
    (t'', e'') <- withVariables (M.toList vars) $ inferExpression expr'
    return ((t', t''), (pat', e'', t''))))

  (ret, xs) <- case tys of
    [] -> E.throwError ("Empty match", Nothing, pos)
    ((_, ret):xs) -> return (ret, xs)

  forM_ xs (\(_, t') -> unify (ret AP.:~: t', pos))

  return (ret, A.EMatch (e', t) cases')
inferExpression (C.Located pos _) =
  E.throwError ("Invalid expression", Nothing, pos)

inferPattern :: M.MonadChecker m => C.Located C.Pattern -> m (T.Type, A.Pattern, M.Map Text T.Scheme)
inferPattern (C.Located _ (C.PVariable (D.Simple name))) = do
  types' <- ST.gets M.types
  case M.lookup name types' of
    Just t -> do
      t' <- M.instantiate t
      return (t', A.PVariable name t', M.empty)
    Nothing -> do
      t <- M.fresh
      return (t, A.PVariable name t, M.singleton name (T.Forall [] t))
inferPattern (C.Located pos (C.PStruct ty fields)) = do
  structs <- ST.gets M.types
  gens <- ST.gets M.generics
  ty' <- P.toWithEnv ty gens
  let name = findName ty'
  case M.lookup name structs of
    Just (T.Forall _ (T.TRec _ fields')) -> do
      (tys, fields'', envs) <-
        unzip3 <$>
        mapM
          (\(C.Annoted name' expr) -> do
             case L.lookup name' fields' of
               Just t -> do
                 (t', expr', env) <- M.local' $ inferPattern expr
                 unify (t AP.:~: t', pos)
                 return (t', expr', env)
               Nothing ->
                 E.throwError ("Field not in struct: " <> name, Nothing, pos))
          fields
      let typedFields = zip (map C.annotedName fields) tys
      return
        ( T.TRec name typedFields
        , A.PStruct ty' (zipWith C.Annoted (map C.annotedName fields) fields'')
        , M.unions envs)
    Just _ -> E.throwError ("Misformed struct: " <> name, Nothing, pos)
    Nothing -> E.throwError ("Struct not in scope: " <> name, Nothing, pos)
inferPattern (C.Located pos (C.PApp (D.Simple name) pats)) = do
  env <- ST.gets M.types
  tv <- M.fresh
  (t, p) <- case M.lookup name env of
    Nothing -> E.throwError ("Unbound constructor: " <> name, Nothing, pos)
    Just scheme -> do
      t <- instantiate scheme
      return (t, name)
  (t', pats', envs) <- L.unzip3 <$> mapM inferPattern pats
  unify (t AP.:~: (t' T.:-> tv), pos)
  return (tv, A.PApp p pats' tv, M.unions envs)
inferPattern (C.Located _ (C.PLiteral lit)) = do
  (t, lit') <- inferLiteral lit
  return (t, A.PLiteral lit', M.empty)
inferPattern (C.Located _ C.PWildcard) = do
  t <- M.fresh
  return (t, A.PWildcard, M.empty)
inferPattern (C.Located pos _) =
  E.throwError ("Invalid pattern", Nothing, pos)

inferLiteral :: a ~ C.Literal => M.Infer a m a
inferLiteral (C.Int i)    = return (T.Int, C.Int i)
inferLiteral (C.Float f)  = return (T.Float, C.Float f)
inferLiteral (C.String s) = return (T.TString, C.String s)
inferLiteral (C.Bool b)   = return (T.Bool, C.Bool b)
inferLiteral (C.Char c)   = return (T.Char, C.Char c)

inferToplevel :: M.Infer (C.Located C.Toplevel) m (Maybe A.Toplevel)
inferToplevel (C.Located pos (C.TFunction gens (C.Annoted name ret) args body)) = do
  generics'' <- mapM (const fresh) gens
  let generics''' = M.fromList $ zip gens generics''
  ret' <- P.toWithEnv ret generics'''
  ST.modify $ \s' -> s' {M.returnType = ret'}
  argsTypes <-
    mapM (\(C.Annoted name' t) -> (name', ) <$> P.toWithEnv t generics''') args
  gens'' <-
    mapM
      (\case
         (T.TVar i) -> return i
         _          -> return (-1))
      generics''
  let gens' = filter (/= -1) gens''
  let funTy = map snd argsTypes T.:-> ret'
  let args' =
        (name, T.Forall [] funTy) : map (BF.second (T.Forall [])) argsTypes
  (t, e') <-
    P.withGenerics
      generics'''
      (withVariables args' $ M.local' $ inferExpression body)
  unify (t AP.:~: ret', pos)
  csts <- ST.gets M.constraints
  s <- CS.solve csts
  ST.modify $ \s' -> s' {M.constraints = []}
  env <- M.ask
  let scheme =
        if null gens
          then generalize env (T.apply s funTy)
          else T.Forall gens' $ T.apply s funTy
  ST.modify $ \s' -> s' {M.variables = M.insert name scheme (M.variables s')}
  ST.modify $ \s' -> s' {M.returnType = T.Void}
  when (name == "main" && not (null (T.free (T.apply s e')))) $ do
    E.throwError ("Main cannot handle generic parameters.", Nothing, pos)
  return
    ( T.Void
    , Just $
      T.apply s $
      A.TFunction
        generics''
        (C.Annoted name ret')
        (map (uncurry C.Annoted) argsTypes)
        e')
inferToplevel (C.Located pos (C.TFunctionProp gens prop (C.Annoted name ret) arguments body)) = do
  generics'' <- mapM (const fresh) gens
  let generics''' = M.fromList $ zip gens generics''
  ret' <- P.toWithEnv ret generics'''
  ST.modify $ \s' -> s' {M.returnType = ret'}
  argsTypes <-
    mapM
      (\(C.Annoted name' t) -> (name', ) <$> P.toWithEnv t generics''')
      arguments
  gens'' <-
    mapM
      (\case
         (T.TVar i) -> return i
         _          -> return (-1))
      generics''
  prop'@(C.Annoted propName propTy) <-
    case prop of
      C.Annoted name' ty -> C.Annoted name' <$> P.toWithEnv ty generics'''
  let gens' = filter (/= -1) gens''
  let funTy = (propTy : map snd argsTypes) T.:-> ret'
  let args' =
        (propName, T.Forall [] propTy) : map (BF.second (T.Forall [])) argsTypes
  ((_, e'), s) <-
    P.withGenerics
      generics'''
      (withClass
         (propTy, name, False)
         (T.Forall gens' funTy)
         (withVariables args' $ M.local' $ do
            res@(t, _) <- inferExpression body
            unify (ret' AP.:~: t, pos)
            csts <- ST.gets M.constraints
            s <- CS.solve csts
            ST.modify $ \s' -> s' {M.constraints = []}
            return (res, s))
         pos)
  
  env <- M.ask
  let scheme' =
        if null gens
          then generalize
                 (T.apply s env)
                 (T.apply s ((propTy : map snd argsTypes) T.:-> ret'))
          else T.Forall (T.apply s gens') $
               T.apply s ((propTy : map snd argsTypes) T.:-> ret')

  ST.modify $ \s' ->
    s'
      { M.returnType = T.Void
      , M.classes = M.insert (T.apply s propTy, name, False) scheme' (M.classes s')
      }
  return
    ( T.Void
    , Just $
      T.apply s $
      A.TFunctionProp
        generics''
        prop'
        (C.Annoted name ret')
        (map (uncurry C.Annoted) argsTypes)
        e')
inferToplevel (C.Located _ (C.TStruct (C.Annoted name gens) fields)) = do
  generics'' <- mapM (const fresh) gens
  let generics''' = M.fromList $ zip gens generics''
  fields' <-
    mapM
      (\(C.Annoted name' ty) -> C.Annoted name' <$> P.toWithEnv ty generics''')
      fields
  let fields'' = map (\(C.Annoted name' ty) -> (name', ty)) fields'
  gens' <-
    mapM
      (\case
         (T.TVar i) -> return i
         _          -> return (-1))
      generics''
  let ty = T.Forall gens' $ T.TRec name fields''
  ST.modify $ \s -> s {M.types = M.insert name ty (M.types s)}
  return (T.Void, Just $ A.TStruct (C.Annoted name generics'') fields')
inferToplevel (C.Located _ (C.TExtern gens (C.Annoted name ty))) = do
  generics'' <- mapM (const fresh) gens
  let generics''' = M.fromList $ zip gens generics''
  ty' <- P.toWithEnv ty generics'''
  envs <- M.ask
  let ty'' = generalize envs ty'
  ST.modify $ \s -> s {M.variables = M.insert name ty'' (M.variables s)}
  return (T.Void, Just $ A.TExtern generics'' (C.Annoted name ty'))
inferToplevel (C.Located _ (C.TProperty gens (C.Annoted _ propTy) (C.Annoted method ret) args)) = do
  generics'' <- mapM (const fresh) gens
  let generics''' = M.fromList $ zip gens generics''
  propTy' <- P.toWithEnv propTy generics'''
  ret' <- P.toWithEnv ret generics'''
  args' <-
    mapM
      (\(C.Annoted name' ty) -> C.Annoted name' <$> P.toWithEnv ty generics''')
      args
  let gens' =
        map
          (\case
             (T.TVar i) -> i
             _          -> (-1))
          generics''
  let scheme = T.Forall gens' ((propTy' : map C.annotedType args') T.:-> ret')
  ST.modify $ \s ->
    s {M.classes = M.insert (propTy', method, True) scheme (M.classes s)}
  return (T.Void, Nothing)
inferToplevel (C.Located _ (C.TEnumeration (C.Annoted name gens) variants)) = do
  gens' <- ST.gets M.generics
  generics'' <- mapM (const fresh) gens
  let generics''' = M.union gens' $ M.fromList $ zip gens generics''
  let header = if null gens then T.TId name else T.TApp (T.TId name) generics''
  variants' <-
    mapM
      (\(C.Annoted name' ty) ->
         C.Annoted name' <$>
         (if null ty
            then pure header
            else (T.:->) <$> mapM (`P.toWithEnv` generics''') ty <*> pure header))
      variants
  let variants'' = map (\(C.Annoted name' ty) -> (name', ty)) variants'
  gens'' <-
    mapM
      (\case
         (T.TVar i) -> return i
         _          -> return (-1))
      generics''
  let tys = map (second (T.Forall gens'')) variants''
  ST.modify $ \s -> s {M.types = M.union (M.types s) (M.fromList tys)}
  return (T.Void, Just $ A.TEnumeration (C.Annoted name generics'') variants')
inferToplevel (C.Located _ (C.TTypeAlias (C.Annoted name gens) ty)) = do
  generics'' <- mapM (const fresh) gens
  let generics''' = M.fromList $ zip gens generics''
  ty' <- P.toWithEnv ty generics'''
  let gens' =
        map
          (\case
             (T.TVar i) -> i
             _          -> (-1))
          generics''
  let ty'' = T.Forall gens' ty'
  ST.modify $ \s -> s {M.aliases = M.insert name ty'' (M.aliases s)}
  return (T.Void, Nothing)
inferToplevel (C.Located pos _) =
  E.throwError ("Invalid toplevel", Nothing, pos)

inferUpdate :: M.Infer (C.Located C.UpdateExpression) m A.UpdateExpression
inferUpdate (C.Located pos (C.UVariable (D.Simple name))) = do
  env <- ST.gets M.variables
  case M.lookup name env of
    Nothing -> E.throwError ("Variable " <> name <> " not found", Nothing, pos)
    Just ty -> do
      ty' <- instantiate ty
      return (ty', A.UVariable name ty')
inferUpdate (C.Located pos (C.UProperty obj name)) = do
  tv <- M.fresh
  (t, e') <- M.local' $ inferUpdate obj
  unify (AP.Field name tv t, pos)
  return (tv, A.UProperty e' name)
inferUpdate (C.Located pos (C.UIndex obj index)) = do
  tv <- M.fresh
  (t, e') <- M.local' $ inferUpdate obj
  unify (t AP.:~: T.TAddr tv, pos)
  (t', e'') <- M.local' $ inferExpression index
  unify (t' AP.:~: T.Int, pos)
  return (tv, A.UIndex e' e'')
inferUpdate (C.Located pos (C.UDereference obj)) = do
  tv <- M.fresh
  (t, e') <- M.local' $ inferUpdate obj
  unify (t AP.:~: T.TAddr tv, pos)
  return (tv, A.UDereference e')
inferUpdate (C.Located pos _) =
  E.throwError ("Invalid update expression", Nothing, pos)

createType :: [T.Type] -> T.Type
createType [] = T.Void
createType xs = fromMaybe T.Void (viaNonEmpty last xs)
