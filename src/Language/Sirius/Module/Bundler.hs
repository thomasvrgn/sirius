{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.Sirius.Module.Bundler where
import Language.Sirius.Module.Monad
import Language.Sirius.CST.Expression
import Language.Sirius.CST.Modules.Located

import qualified Control.Monad.Except as E
import qualified Data.Map as M
import qualified Control.Monad.State as ST
import qualified Language.Sirius.CST.Modules.Type as D
import qualified Language.Sirius.CST.Modules.Annoted as C

import Data.Functor
import Prelude hiding (local)
import qualified Language.Sirius.CST.Modules.Namespaced as D
import qualified Data.Text as L
import Data.List (nub)

local' :: MonadBundling m => (BundlingState -> BundlingState) -> m a -> m a
local' f m = do
  s <- ST.get
  ST.modify f
  a <- m
  ST.modify $ \s' -> s' { mappings = mappings s' `M.union` mappings s, types = types s' `M.union` types s }
  return a

local :: MonadBundling m => (BundlingState -> BundlingState) -> m a -> m a
local f m = do
  s <- ST.get
  ST.modify f
  a <- m
  ST.modify $ \s' -> s' { mappings = mappings s, types = types s, currentPaths = currentPaths s }
  return a

createName :: MonadBundling m => Text -> m Text
createName name = do
  paths <- ST.gets currentPaths
  return $ (if not (null paths) then L.intercalate "::" paths <> "::" else "") <> name

analyseToplevel :: MonadBundling m => Located Toplevel -> m [Located Toplevel]
analyseToplevel (TAnnotation name tl :>: pos) = do
  name' <- createName name
  tl' <- analyseToplevel tl
  return (map (\tl -> TAnnotation name' tl :>: pos) tl')
analyseToplevel (TFunction gens (C.Annoted name ret) args body :>: pos) = do
  (name', new) <- do
    if name == "main"
      then return ("main", "main")
      else do
        name' <- createName name
        return (name', name')
  let args' = map C.annotedName args
  tys <- mapM ((`resolveImportedType` pos) . C.annotedType) args
  ret <- resolveImportedType ret pos
  let env' = M.fromList $ zip args' args'
  body' <- local' (\s -> s { mappings = M.insert name' new (M.union env' (mappings s)) }) $
    resolveImportedExpressions body
  return [TFunction gens (C.Annoted new ret) (zipWith C.Annoted args' tys) body' :>: pos]
analyseToplevel (TNamespace name toplevels :>: _) =
  case name of
    "_" ->
      local' (const emptyBundling) $ concat <$> mapM analyseToplevel toplevels
    _ -> do
      ST.modify $ \s -> s { currentPaths = currentPaths s ++ [name] }
      toplevels' <- concat <$> mapM analyseToplevel toplevels
      ST.modify $ \s -> s { currentPaths = case currentPaths s of
        [] -> []
        _ -> fromMaybe [] (viaNonEmpty init (currentPaths s)) }
      return toplevels'
analyseToplevel (TStruct (C.Annoted name gens) fields :>: pos) = do
  fieldsTy <- mapM ((`resolveImportedType` pos) . C.annotedType) fields
  name' <- createName name
  let fields' = map C.annotedName fields
  let env' = M.fromList $ zip fields' fields'
  ST.modify $ \s -> s { types = M.insert name' name' (M.union env' (mappings s)) }
  return [TStruct (C.Annoted name' gens) (zipWith C.Annoted fields' fieldsTy) :>: pos]
analyseToplevel (TExtern gens (C.Annoted name ty) :>: pos) = do
  name' <- createName name
  ty'' <- resolveImportedType ty pos
  let env' = M.fromList [(name, name)]
  ST.modify $ \s -> s { mappings = M.insert name' name' (M.union env' (mappings s)) }
  return [TExtern gens (C.Annoted name' ty'')  :>: pos]
analyseToplevel (TFunctionProp gens (C.Annoted propName propTy) (C.Annoted name ret) args body :>: pos) = do
  name' <- createName name
  propTy' <- resolveImportedType propTy pos
  ret' <- resolveImportedType ret pos
  let args' = map C.annotedName args
  tys <- mapM ((`resolveImportedType` pos) . C.annotedType) args
  let env' = M.fromList $ zip args' args'
  body' <- local' (\s -> s { mappings = M.fromList [(name', name'), (propName, propName)] `M.union` M.union env' (mappings s) }) $
    resolveImportedExpressions body
  return [TFunctionProp gens (C.Annoted propName propTy') (C.Annoted name' ret') (zipWith C.Annoted args' tys) body' :>: pos]
analyseToplevel (TProperty gens (C.Annoted propName propTy) (C.Annoted name ty) args :>: pos) = do
  name' <- createName name
  propTy' <- resolveImportedType propTy pos
  ty' <- resolveImportedType ty pos
  let args' = map C.annotedName args
  tys <- mapM ((`resolveImportedType` pos) . C.annotedType) args
  return [TProperty gens (C.Annoted propName propTy') (C.Annoted name' ty') (zipWith C.Annoted args' tys) :>: pos]
analyseToplevel (TEnumeration (C.Annoted name gens) constructors :>: pos) = do
  name' <- createName name
  let constructors' = map C.annotedName constructors
  let env' = M.fromList $ zip constructors' constructors'
  ST.modify $ \s -> s { types = M.insert name' name' (M.union env' (mappings s)) }
  
  ST.modify $ \s -> s { mappings = M.union env' (mappings s) }
  return [TEnumeration (C.Annoted name' gens) constructors :>: pos]
analyseToplevel x = return [x]

keep :: Ord k => M.Map k a -> [k] -> M.Map k a
keep m ks = M.fromList $ filter (\(k, _) -> k `elem` ks) $ M.toList m

lookupModule :: MonadBundling m => Maybe [Text] -> [Located Toplevel] -> m [Located Toplevel]
lookupModule elements ast = do
  ast'' <- local' (\s -> s { mappings = M.empty }) $ do
    xs <- mapM analyseToplevel ast
    ST.modify $ \s -> s { mappings = keep (mappings s) (case elements of
      Just xs' -> xs'
      _ -> M.keys $ mappings s) }
    return xs
  return $ concat ast''

makeName :: D.Namespaced -> Text
makeName (D.Namespaced paths name) = (if not (null paths) then L.intercalate "::" paths <> "::" else "") <> name
makeName (D.Simple name) = name

resolveImportedType :: MonadBundling m => D.Type -> Position -> m D.Type
resolveImportedType (D.TypeVar name) _ = do
  mappings' <- ST.gets types
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ D.TypeVar (D.Simple name')
    Nothing -> return $ D.TypeVar name
resolveImportedType (D.TypeApp name args) pos = do
  args' <- mapM (`resolveImportedType` pos) args
  return $ D.TypeApp name args'
resolveImportedType (D.TypeFunction args ret) pos = do
  args' <- mapM (`resolveImportedType` pos) args
  ret' <- resolveImportedType ret pos
  return $ D.TypeFunction args' ret'
resolveImportedType x _ = return x

resolveImportedMaybeType :: MonadBundling m => Maybe D.Type -> Position -> m (Maybe D.Type)
resolveImportedMaybeType Nothing _ = return Nothing
resolveImportedMaybeType (Just t) pos = do
  t' <- resolveImportedType t pos
  return $ Just t'

resolveImportedExpressions :: MonadBundling m => Located Expression -> m (Located Expression)
resolveImportedExpressions (EVariable name :>: pos) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ EVariable (D.Simple name') :>: pos
    Nothing -> E.throwError ("Variable " <> show name <> " is not defined", pos)
resolveImportedExpressions (EApplication n args :>: pos) = do
  n' <- resolveImportedExpressions n
  args' <- mapM resolveImportedExpressions args
  return $ EApplication n' args' :>: pos
resolveImportedExpressions (EFunction ret args body :>: pos) = do
  let names = map C.annotedName args
  tys <- mapM ((`resolveImportedType` pos) . C.annotedType) args
  body' <- local' (\s -> s { mappings = M.union (M.fromList $ zip names names) (mappings s) }) $
    resolveImportedExpressions body
  ret' <- resolveImportedType ret pos
  return $ EFunction ret' (zipWith C.Annoted names tys) body' :>: pos
resolveImportedExpressions (EIf cond t f :>: pos) = do
  cond' <- resolveImportedExpressions cond
  t' <- mapM resolveImportedExpressions t
  f' <- mapM resolveImportedExpressions f
  return $ EIf cond' t' f' :>: pos
resolveImportedExpressions (EBlock exprs :>: pos) = do
  exprs' <- local id $ mapM resolveImportedExpressions exprs
  return $ EBlock exprs' :>: pos
resolveImportedExpressions (EList exprs :>: pos) = do
  exprs' <- mapM resolveImportedExpressions exprs
  return $ EList exprs' :>: pos
resolveImportedExpressions (EIndex arr index :>: pos) = do
  arr' <- resolveImportedExpressions arr
  index' <- resolveImportedExpressions index
  return $ EIndex arr' index' :>: pos
resolveImportedExpressions (ELet (C.Annoted n t) expr body :>: pos) = do
  ST.modify (\s -> s { mappings = M.insert n n (mappings s) })
  t <- resolveImportedType t pos
  expr' <- resolveImportedExpressions expr
  body' <- case body of
    Just body' -> Just <$> resolveImportedExpressions body'
    Nothing -> return Nothing
  return $ ELet (C.Annoted n t) expr' body' :>: pos
resolveImportedExpressions (EWhile cond body :>: pos) = do
  cond' <- resolveImportedExpressions cond
  body' <- mapM resolveImportedExpressions body
  return $ EWhile cond' body' :>: pos
resolveImportedExpressions (EFor n from to body :>: pos) = do
  from' <- resolveImportedExpressions from
  to' <- resolveImportedExpressions to
  body' <- local' (\s -> s { mappings = M.insert n n (mappings s) }) $ mapM resolveImportedExpressions body
  return $ EFor n from' to' body' :>: pos
resolveImportedExpressions (EUpdate update expr :>: pos) = do
  update' <- resolveImportedUpdate update
  expr' <- resolveImportedExpressions expr
  return $ EUpdate update' expr' :>: pos
resolveImportedExpressions (ELiteral l :>: pos) = return $ ELiteral l :>: pos
resolveImportedExpressions (EStruct n fields :>: pos) = do
  n' <- resolveImportedType n pos
  fields' <- mapM (\(C.Annoted name expr) -> C.Annoted name <$> resolveImportedExpressions expr) fields
  return $ EStruct n' fields' :>: pos
resolveImportedExpressions (EProperty expr name :>: pos) = do
  expr' <- resolveImportedExpressions expr
  return $ EProperty expr' name :>: pos
resolveImportedExpressions (EDereference expr :>: pos) = do
  expr' <- resolveImportedExpressions expr
  return $ EDereference expr' :>: pos
resolveImportedExpressions (EReference expr :>: pos) = do
  expr' <- resolveImportedExpressions expr
  return $ EReference expr' :>: pos
resolveImportedExpressions (ESizeOf t :>: pos) = do
  t' <- resolveImportedType t pos
  return $ ESizeOf t' :>: pos
resolveImportedExpressions (EAnnotation expr t :>: pos) = do
  expr' <- resolveImportedExpressions expr
  t' <- resolveImportedType t pos
  return $ EAnnotation expr' t' :>: pos
resolveImportedExpressions (EAssembly op exprs :>: pos) = do
  exprs' <- mapM resolveImportedExpressions exprs
  return $ EAssembly op exprs' :>: pos
resolveImportedExpressions (EMatch expr cases :>: pos) = do
  expr' <- resolveImportedExpressions expr
  cases' <- mapM (\(pat, expr) -> local id $ (,) <$> resolveImportedPattern pat <*> resolveImportedExpressions expr) cases
  return $ EMatch expr' cases' :>: pos
resolveImportedExpressions (Located pos _) = E.throwError ("Not implemented", pos)

resolveImportedUpdate :: MonadBundling m => Located UpdateExpression -> m (Located UpdateExpression)
resolveImportedUpdate (UVariable name :>: pos) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ UVariable (D.Simple name') :>: pos
    Nothing -> return $ UVariable name :>: pos
resolveImportedUpdate (UIndex arr index :>: pos) = do
  arr' <- resolveImportedUpdate arr
  index' <- resolveImportedExpressions index
  return $ UIndex arr' index' :>: pos
resolveImportedUpdate (UProperty expr name :>: pos) = do
  expr' <- resolveImportedUpdate expr
  return $ UProperty expr' name :>: pos
resolveImportedUpdate (UDereference e :>: pos) = do
  e' <- resolveImportedUpdate e
  return $ UDereference e' :>: pos
resolveImportedUpdate (Located pos _) = E.throwError ("Not implemented", pos)

resolveImportedPattern :: MonadBundling m => Located Pattern -> m (Located Pattern)
resolveImportedPattern (PVariable name :>: pos) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ PVariable (D.Simple name') :>: pos
    Nothing -> do
      ST.modify (\s -> s { mappings = M.insert (makeName name) (makeName name) (mappings s) })
      return $ PVariable name :>: pos
resolveImportedPattern (PStruct n fields :>: pos) = do
  n' <- resolveImportedType n pos
  fields' <- mapM (\(C.Annoted name pat) -> C.Annoted name <$> resolveImportedPattern pat) fields
  return $ PStruct n' fields' :>: pos
resolveImportedPattern (PApp name pats :>: pos) = do
  mappings' <- ST.gets mappings
  name' <- case M.lookup (makeName name) mappings' of
    Just name' -> return $ D.Simple name'
    Nothing -> return name
  pats' <- mapM resolveImportedPattern pats
  return $ PApp name' pats' :>: pos
resolveImportedPattern (PWildcard :>: pos) = return (PWildcard :>: pos)
resolveImportedPattern (PLiteral l :>: pos) = return $ PLiteral l :>: pos
resolveImportedPattern (Located pos _) = E.throwError ("Not implemented", pos)

runModuleBundling :: (E.MonadIO m, MonadFail m) => [Located Toplevel] -> m (Either (Text, Position) [Located Toplevel])
runModuleBundling toplevel =
  E.runExceptT (ST.evalStateT (nub <$> lookupModule Nothing toplevel) (BundlingState [] mempty mempty 0 []))