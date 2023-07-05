
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}


module Language.Sirius.Monomorphize where

import qualified Control.Monad.Except                           as E
import qualified Control.Monad.State                            as ST
import qualified Data.Map                                       as M
import qualified Data.Set                                       as S
import qualified Data.Text                                      as L
import qualified Language.Sirius.CST.Modules.Annoted            as C
import qualified Language.Sirius.CST.Modules.Located            as C
import qualified Language.Sirius.Typecheck.Checker              as Checker
import qualified Language.Sirius.Typecheck.Definition.AST       as A
import qualified Language.Sirius.Typecheck.Definition.Type      as T
import qualified Language.Sirius.Typecheck.Modules.Substitution as Sub
import qualified Language.Sirius.Typecheck.Modules.Unification  as U
import qualified Language.Sirius.Typecheck.Modules.Apply as Sub
import Language.Sirius.Typecheck.ConstraintSolver (findM)
import qualified Language.Sirius.Typecheck.Definition.Monad as Checker

data MonomorphizationState =
  MonomorphizationState
    { mstTypeMap      :: M.Map (Text, T.Type) Text
    , mstVarExclusion :: S.Set Text
    , mstVarDefs      :: M.Map Text A.Toplevel
    , mstToplevels    :: [A.Toplevel]
    , mstTypes        :: M.Map T.Type Text
    , mstTypeDefs     :: M.Map Text A.Toplevel
    , mstProps        :: M.Map (Text, T.Type) Text
    , mstPropDefs     :: M.Map Text A.Toplevel
    , mstCheckerState :: Checker.CheckerState
    }

emptyState :: MonomorphizationState
emptyState =
  MonomorphizationState
    { mstTypeMap = mempty
    , mstVarExclusion = mempty
    , mstVarDefs = mempty
    , mstToplevels = mempty
    , mstTypes = mempty
    , mstTypeDefs = mempty
    , mstProps = mempty
    , mstPropDefs = mempty
    , mstCheckerState = mempty
    }

type MonadMono m = (MonadState MonomorphizationState m, E.MonadError (Text, Maybe Text, C.Position) m)

monoToplevel :: MonadMono m => [A.Toplevel] -> m [A.Toplevel]
monoToplevel (A.TExtern gens (C.Annoted name ty):xs) = do
  ty' <- monoType ty
  ST.modify $ \s ->
    s
      { mstVarExclusion = S.insert name (mstVarExclusion s)
      , mstVarDefs =
          M.insert name (A.TExtern gens (C.Annoted name ty')) (mstVarDefs s)
      }
  monoToplevel xs
monoToplevel (A.TFunction gens (C.Annoted name ret) args body:xs) =
  if name == "main"
    then do
      ST.modify $ \s -> s {mstVarExclusion = S.insert name (mstVarExclusion s)}
      body' <- monoExpr body
      xs' <- monoToplevel xs
      return $ A.TFunction gens (C.Annoted name ret) args body' : xs'
    else monoToplevel xs
monoToplevel (_:xs) = monoToplevel xs
monoToplevel [] = return []

findToplevel :: MonadMono m => Text -> m (Maybe A.Toplevel)
findToplevel name = do
  m <- ST.gets mstToplevels
  return $
    find
      (\case
         A.TExtern _ (C.Annoted _ _)           -> False
         A.TFunction _ (C.Annoted name' _) _ _ -> name == name'
         _                                     -> False)
      m

lookupFuncProp :: MonadMono m => Text -> T.Type -> m (Maybe Text)
lookupFuncProp name ty = do
  m <- ST.gets mstTypeMap
  xs <- findM (\((name', ty'), _) -> (name == name' &&) . isRight <$> runMGUM ty' ty) (M.toList m)
  case xs of
    Just ((_, _), name') -> return $ Just name'
    Nothing -> return Nothing

findFuncProp :: MonadMono m => Text -> T.Type -> m (Maybe A.Toplevel)
findFuncProp name ty = do
  m <- ST.gets mstToplevels
  findM
    (\case
        A.TFunctionProp _ (C.Annoted _ ty') (C.Annoted name' _) _ _ -> do
          let bool' = name == name'
          bool'' <- runMGUM ty ty'
          case bool'' of
            Left _ -> return False
            Right (Left _) -> return False
            Right (Right _) -> return bool'
        _ -> return False)
    m

runMGU ::
     Monad m
  => T.Type
  -> T.Type
  -> m (Either (Text, Maybe Text, C.Position) (Either Text Sub.Substitution))
runMGU t1 t2 =
  E.runExceptT $ ST.evalStateT (U.mgu t1 t2) mempty

runMGUM ::
     MonadMono m
  => T.Type
  -> T.Type
  -> m (Either (Text, Maybe Text, C.Position) (Either Text Sub.Substitution))
runMGUM t1 t2 = do
  functions' <- ST.gets (Checker.variables . mstCheckerState)
  types' <- ST.gets (Checker.types . mstCheckerState)
  E.runExceptT $ ST.evalStateT (U.mgu t1 t2) (mempty {Checker.variables = functions', Checker.types = types'})

findStruct :: MonadMono m => T.Type -> m (Maybe A.Toplevel)
findStruct name = do
  m <- ST.gets mstToplevels
  let name' = Checker.findName name
  return $
    find
      (\case
         A.TStruct (C.Annoted name'' _) _ -> name'' == name'
         A.TEnumeration (C.Annoted name'' _) _ -> name'' == name'
         _                                -> False)
      m

monoExpr :: MonadMono m => A.Expression -> m A.Expression
monoExpr (A.ELocated (A.EVariable name ty) pos) = do
  m <- ST.gets mstTypeMap
  case M.lookup (name, ty) m of
    Just name' -> A.EVariable name' <$> monoType ty
    Nothing -> do
      found <- findToplevel name
      case found of
        Just (A.TFunction _ (C.Annoted name' retTy) args body) -> do
          let argsTy = map C.annotedType args
          let funTy = argsTy T.:-> retTy
          sub <- runMGUM ty funTy
          case sub of
            Left (err, _, _) -> E.throwError (err, Nothing, pos)
            Right (Left err) -> E.throwError (err, Nothing, pos)
            Right (Right sub') -> do
              let name'' =
                    name' <> "_" <> L.intercalate "_" (map show (M.elems sub'))
              body' <- monoExpr (Sub.apply sub' body)
              retTy' <- monoType $ Sub.apply sub' retTy
              args' <-
                zipWith C.Annoted (map C.annotedName args) <$>
                mapM monoType (Sub.apply sub' argsTy)
              ST.modify $ \s ->
                s
                  { mstVarDefs =
                      M.insert
                        name''
                        (Sub.apply sub' $
                         A.TFunction [] (C.Annoted name'' retTy') args' body')
                        (mstVarDefs s)
                  , mstTypeMap = M.insert (name, ty) name'' (mstTypeMap s)
                  }
              A.EVariable name'' <$> monoType ty
        _ -> A.EVariable name <$> monoType ty
monoExpr (A.EApplication fun args ret) = do
  fun' <- monoExpr fun
  args' <- mapM monoExpr args
  ret' <- monoType ret
  return $ A.EApplication fun' args' ret'
monoExpr (A.EIf cond then' else') = do
  cond' <- monoExpr cond
  then'' <- mapM monoExpr then'
  else'' <- mapM monoExpr else'
  return $ A.EIf cond' then'' else''
monoExpr (A.EBlock exprs) = do
  exprs' <- mapM monoExpr exprs
  return $ A.EBlock exprs'
monoExpr (A.EWhile cond body) = do
  cond' <- monoExpr cond
  body' <- mapM monoExpr body
  return $ A.EWhile cond' body'
monoExpr (A.EFor name from to body) = do
  from' <- monoExpr from
  to' <- monoExpr to
  body' <- mapM monoExpr body
  return $ A.EFor name from' to' body'
monoExpr (A.EFunction ret args body) = do
  ret' <- monoType ret
  args' <- mapM (\(C.Annoted name ty) -> C.Annoted name <$> monoType ty) args
  body' <- monoExpr body
  return $ A.EFunction ret' args' body'
monoExpr (A.ELet (C.Annoted name ty) expr body t) = do
  expr' <- monoExpr expr
  ty' <- monoType ty
  t' <- case t of
    Just t' -> Just <$> monoType t'
    Nothing -> return Nothing
  body' <- maybe (return Nothing) ((Just <$>) . monoExpr) body
  return $ A.ELet (C.Annoted name ty') expr' body' t'
monoExpr (A.EProperty expr field) = do
  expr' <- monoExpr expr
  return $ A.EProperty expr' field
monoExpr (A.EList exprs t) = do
  exprs' <- mapM monoExpr exprs
  return $ A.EList exprs' t
monoExpr (A.EStruct name fields) = do
  m <- ST.gets mstTypes
  exprs <- mapM (\(C.Annoted name' e) -> C.Annoted name' <$> monoExpr e) fields
  case M.lookup name m of
    Just name' -> return $ A.EStruct (T.TId name') exprs
    Nothing -> do
      found <- findStruct name
      case name of
        T.TApp _ xs -> do
          case found of
            Just (A.TStruct (C.Annoted name' gens) fields') -> do
              let gens' = map (\(T.TVar name'') -> name'') gens
              let sub = M.fromList $ zip gens' xs
              let subName = name' <> "_" <> L.intercalate "_" (map show xs)
              ST.modify $ \s ->
                s
                  { mstTypes = M.insert name subName (mstTypes s)
                  , mstTypeDefs =
                      M.insert
                        subName
                        (A.TStruct
                           (C.Annoted subName [])
                           (Sub.apply sub fields'))
                        (mstTypeDefs s)
                  }
              return $ A.EStruct (T.TId subName) exprs
            _ -> return $ A.EStruct name exprs
        _ -> return $ A.EStruct name exprs
monoExpr (A.EIndex expr index) = do
  expr' <- monoExpr expr
  index' <- monoExpr index
  return $ A.EIndex expr' index'
monoExpr (A.ELocated (A.EClassVariable name ty app) pos) = do
  foundPropName <- lookupFuncProp name ty
  case foundPropName of
    Nothing -> do
      found <- findFuncProp name ty
      case found of
        Nothing -> E.throwError ("No function property " <> name <> " found on type " <> show ty, Nothing, pos)
        Just (A.TFunctionProp _ (C.Annoted propName ty') (C.Annoted funcName ret) args body) -> do
          sub <- runMGUM ty ty'
          case sub of
            Left (err, _, _) -> error err
            Right (Left err) -> error err
            Right (Right sub') -> do
              let funTy = map C.annotedType args T.:-> ret
              sub2 <- runMGUM app funTy
              case sub2 of
                Left (err, _, _) -> error err
                Right (Left err) -> error err
                Right (Right sub3) -> do
                  let sub4 = Sub.compose sub' sub3
                  ty'' <- monoType (Sub.apply sub4 ty')
                  let name' =
                        "$" <> funcName <> "_" <> show (Sub.apply sub4 (case app of
                            args' T.:-> ret' -> (ty:args') T.:-> ret'
                            _ -> app))
                  body' <- monoExpr (Sub.apply sub4 body)
                  ret' <- monoType (Sub.apply sub4 ret)
                  args' <-
                    zipWith C.Annoted (map C.annotedName args) <$>
                    mapM monoType (Sub.apply sub4 (map C.annotedType args))
                  propType <- monoType (Sub.apply sub4 ty')
                  ST.modify $ \s ->
                    s
                      { mstPropDefs =
                          M.insert
                            name'
                            (Sub.apply sub4 $
                             A.TFunction [] (C.Annoted name' ret') (C.Annoted propName propType : args') body')
                            (mstPropDefs s)
                      , mstProps = M.insert (name, ty) name' (mstProps s)
                      }
                  return $ A.EVariable name' ((ty'' : map C.annotedType args) T.:-> ret')
        Just _ -> error "COMPILER ERROR: Function property not found through the toplevels"
    Just name' -> case app of
      args T.:-> ret -> return $ A.EVariable name' ((ty:args) T.:-> ret)
      _ -> return $ A.EVariable name' ty
monoExpr (A.EDereference expr) = do
  expr' <- monoExpr expr
  return $ A.EDereference expr'
monoExpr (A.EReference expr) = do
  expr' <- monoExpr expr
  return $ A.EReference expr'
monoExpr (A.ESizeOf t) = A.ESizeOf <$> monoType t
monoExpr (A.ELiteral l) = return $ A.ELiteral l
monoExpr (A.EUpdate update expr) = do
  update' <- monoUpdate update
  expr' <- monoExpr expr
  return $ A.EUpdate update' expr'
monoExpr (A.EAssembly op exprs) = do
  exprs' <- mapM monoExpr exprs
  return $ A.EAssembly op exprs'
monoExpr (A.ELocated expr _) = monoExpr expr
monoExpr A.EVariable {} = error "COMPILER ERROR: EVariable should not be in the AST at this point"
monoExpr A.EClassVariable {} = error "COMPILER ERROR: EClassVariable should not be in the AST at this point"
monoExpr (A.EInternalField expr f) = do
  expr' <- monoExpr expr
  return $ A.EInternalField expr' f
monoExpr (A.EDeclaration name ty) = do
  ty' <- monoType ty
  return $ A.EDeclaration name ty'

monoUpdate :: MonadMono m => A.UpdateExpression -> m A.UpdateExpression
monoUpdate (A.UVariable name ty) = do
  m <- ST.gets mstTypeMap
  case M.lookup (name, ty) m of
    Just name' -> return $ A.UVariable name' ty
    Nothing    -> return $ A.UVariable name ty
monoUpdate (A.UDereference expr) = do
  expr' <- monoUpdate expr
  return $ A.UDereference expr'
monoUpdate (A.UIndex expr index) = do
  expr' <- monoUpdate expr
  index' <- monoExpr index
  return $ A.UIndex expr' index'
monoUpdate (A.UProperty expr field) = do
  expr' <- monoUpdate expr
  return $ A.UProperty expr' field
monoUpdate (A.UInternalField expr field) = do
  expr' <- monoUpdate expr
  return $ A.UInternalField expr' field


runMonomorphizationPass :: Monad m => [A.Toplevel] -> Checker.CheckerState -> m (Either (Text, Maybe Text, C.Position) [A.Toplevel])
runMonomorphizationPass xs c = do
  res <- E.runExceptT $ ST.runStateT (monoToplevel xs) emptyState {mstToplevels = xs, mstCheckerState = c}
  case res of
    Left err -> return $ Left err
    Right (xs', st) -> do
      let tys = M.elems $ mstTypeDefs st
      let xs'' = M.elems $ mstVarDefs st
      let props = M.elems $ mstPropDefs st
      return . Right $ tys <> xs'' <> props <> xs'

monoType :: MonadMono m => T.Type -> m T.Type
monoType (args T.:-> ret) = do
  ret' <- monoType ret
  args' <- mapM monoType args
  return $ args' T.:-> ret'
monoType z@(T.TId n) = do
  m <- ST.gets mstTypes
  case M.lookup z m of
    Just name' -> return $ T.TId name'
    Nothing -> do
      found <- findStruct z
      case found of
        Just (A.TStruct (C.Annoted _ _) xs') -> do
          ST.modify $ \s -> s {mstTypes = M.insert z n (mstTypes s)}
          ST.modify $ \s ->
            s
              { mstTypeDefs =
                  M.insert
                    n
                    (A.TStruct (C.Annoted n []) xs')
                    (mstTypeDefs s)
              }
          return $ T.TId n
        Just (A.TEnumeration (C.Annoted _ _) xs') -> do
          ST.modify $ \s -> s {mstTypes = M.insert z n (mstTypes s)}
          ST.modify $ \s ->
            s
              { mstTypeDefs =
                  M.insert
                    n
                    (A.TEnumeration (C.Annoted n []) xs')
                    (mstTypeDefs s)
              }
          return $ T.TId n
        _ -> return z
monoType z@(T.TApp n args) = do
  m <- ST.gets mstTypes
  case M.lookup z m of
    Just name' ->
      return $ T.TId name'
    Nothing -> do
      found <- findStruct n
      case found of
        Just (A.TStruct (C.Annoted name' gens) xs') -> do
          let gens' = map (\(T.TVar name'') -> name'') gens
          let sub = M.fromList $ zip gens' args
          let subName = name' <> "_" <> L.intercalate "_" (map show args)
          ST.modify $ \s -> s {mstTypes = M.insert z subName (mstTypes s)}
          fields' <-
            mapM
              (\(C.Annoted name'' ty) ->
                C.Annoted name'' <$> monoType (Sub.apply sub ty))
              xs'
          ST.modify $ \s ->
            s
              { mstTypeDefs =
                  M.insert
                    subName
                    (A.TStruct (C.Annoted subName []) fields')
                    (mstTypeDefs s)
              }
          return $ T.TId subName
        Just (A.TEnumeration (C.Annoted name' gens) xs') -> do
          let gens' = map (\(T.TVar name'') -> name'') gens
          let sub = M.fromList $ zip gens' args
          let subName = name' <> "_" <> L.intercalate "_" (map show args)
          ST.modify $ \s -> s {mstTypes = M.insert z subName (mstTypes s)}
          fields' <-
            mapM
              (\(C.Annoted name'' ty) ->
                C.Annoted name'' <$> monoType (Sub.apply sub ty))
              xs'
          ST.modify $ \s ->
            s
              { mstTypeDefs =
                  M.insert
                    subName
                    (A.TEnumeration (C.Annoted subName []) fields')
                    (mstTypeDefs s)
              }
          return $ T.TId subName
        _ -> do
          name <- monoType n
          xs <- mapM monoType args
          return $ T.TApp name xs
monoType x = return x
