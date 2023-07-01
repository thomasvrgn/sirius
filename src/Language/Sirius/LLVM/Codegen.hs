{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Sirius.LLVM.Codegen where

import qualified Control.Monad.State                         as ST
import qualified Data.Map                                    as M
import           Data.Maybe                                  (fromJust)
import qualified Language.Sirius.CST.Modules.Annoted         as C
import qualified Language.Sirius.CST.Modules.Literal         as L
import           Language.Sirius.LLVM.Modules.Monad          (LLVM,
                                                              LLVMState (lsAliases, lsEnv, lsStructs),
                                                              fresh)
import           Language.Sirius.LLVM.Modules.StructDebrujin (toDebrujinStruct)
import           Language.Sirius.LLVM.Modules.Type           (fromType, toBS)
import qualified Language.Sirius.ANF.AST    as T
import qualified Language.Sirius.Typecheck.Definition.Type   as T
import qualified LLVM.AST                                    as AST
import qualified LLVM.AST.Constant                           as AST
import qualified LLVM.AST.FloatingPointPredicate             as FP
import qualified LLVM.AST.IntegerPredicate                   as IP
import qualified LLVM.AST.Type                               as AST
import qualified LLVM.AST.Typed                              as AST
import qualified LLVM.IRBuilder                              as AST
import qualified LLVM.IRBuilder                              as IRB
import qualified Data.List as L

string :: LLVM m => String -> m AST.Operand
string s = do
  i <- fresh
  let name = "string" ++ show i
  AST.ConstantOperand <$> IRB.globalStringPtr s (AST.Name $ fromString name)

declare :: LLVM m => [T.Toplevel] -> m ()
declare (T.TFunction (C.Annoted name ret) args _:xs) = do
  unless (name == "main") $ do
    ret' <- fromType ret
    args' <- mapM (fromType . C.annotedType) args
    name' <-
      IRB.function
        (AST.Name $ toBS name)
        (zip args' (map (AST.ParameterName . toBS . C.annotedName) args))
        ret' $
      const (return ())
    ST.modify $ \s -> s {lsEnv = M.insert name name' (lsEnv s)}
  declare xs
declare (z@T.TStruct {}:xs) = toDebrujinStruct z *> declare xs
declare (T.TExtern (C.Annoted name ty):xs) = do
  name' <-
    case ty of
      args T.:-> ret -> do
        ret' <- fromType ret
        args' <- mapM fromType args
        IRB.extern (AST.Name $ toBS name) args' ret'
      ty' -> do
        ty'' <- fromType ty'
        IRB.extern (AST.Name $ toBS name) [] ty''
  ST.modify $ \s -> s {lsEnv = M.insert name name' (lsEnv s)}
  declare xs
declare [] = return ()

genToplevel :: LLVM m => T.Toplevel -> m ()
genToplevel (T.TFunction (C.Annoted name ret) args body) = do
  ret' <- fromType ret
  args' <- mapM (fromType . C.annotedType) args
  name' <-
    IRB.function
      (AST.Name $ toBS name)
      (zipWith
         (\ty v -> (ty, AST.ParameterName (toBS v)))
         args'
         (map C.annotedName args))
      ret' $ \args'' -> do
      forM_ (zip3 args'' args' args) $ \(arg, ty, C.Annoted name' _) -> do
        i <- IRB.alloca ty Nothing 0
        IRB.store i 0 arg
        ST.modify $ \s -> s {lsEnv = M.insert name' i (lsEnv s)}
      env <- ST.gets lsEnv
      body' <- genExpression body
      maybe (IRB.ret (IRB.int32 0)) IRB.ret body'
      ST.modify $ \s -> s {lsEnv = env}
  ST.modify $ \s -> s {lsEnv = M.insert name name' (lsEnv s)}
genToplevel _ = return ()

namedBlock :: IRB.MonadIRBuilder m => AST.Name -> m AST.Name
namedBlock nm = do
  IRB.emitBlockStart nm
  return nm

index :: [a] -> Int -> Maybe a
index (x:_) 0  = Just x
index (_:xs) n = index xs (n - 1)
index [] _     = Nothing

genExpression :: LLVM m => T.Expression -> m (Maybe AST.Operand)
genExpression (T.EVariable "void") = return $ Just (IRB.int32 0)
genExpression (T.EVariable name) = do
  env <- ST.gets lsEnv
  case M.lookup name env of
    Just op -> do
      ty <- AST.typeOf op
      case ty of
        Right (AST.PointerType AST.FunctionType {} _) -> return $ Just op
        _                                             -> Just <$> IRB.load op 0
    Nothing -> error $ "genExpression: variable " <> name <> " not found"
genExpression (T.EApplication f args) = do
  env <- ST.gets lsEnv
  let f' = M.lookup f env
  args' <- mapM genExpression args
  case f' of
    Just f'' -> do
      unless (all isJust args') $
        error "genExpression: not all arguments are Just"
      tyF <- AST.typeOf f''
      tyArgs <- mapM (AST.typeOf . fromJust) args'
      args'' <-
        case tyF of
          Right (AST.PointerType (AST.FunctionType _ args'' _) _) ->
            forM (zip [(0 :: Int) ..] args'') $ \(idx, arg) -> do
              let Right tyArg = fromJust $ index tyArgs idx
              let arg' = fromJust . fromJust $ index args' idx
              if tyArg == arg
                then return arg'
                else bitcast arg' arg
          x -> error $ "genExpression: not a function: " <> show x
      Just <$> IRB.call f'' (zip args'' (repeat []))
    Nothing -> error $ "genExpression: function " <> f <> " not found"
genExpression (T.EBlock exprs) = do
  env <- ST.gets lsEnv
  exprs' <- mapM genExpression exprs
  ST.modify $ \s -> s {lsEnv = env}
  return (fromJust $ viaNonEmpty last exprs')
genExpression (T.EIf cond then' else') = do
  thenLabel <- IRB.freshName "then"
  elseLabel <- IRB.freshName "else"
  mergeLabel <- IRB.freshName "merge"
  cond' <- genExpression cond
  IRB.condBr (fromJust cond') thenLabel elseLabel
  namedBlock thenLabel
  then'' <- viaNonEmpty last . catMaybes <$> mapM genExpression then'
  IRB.br mergeLabel
  namedBlock elseLabel
  else'' <- viaNonEmpty last . catMaybes <$> mapM genExpression else'
  IRB.br mergeLabel
  namedBlock mergeLabel
  if isNothing then'' || isNothing else''
    then return Nothing
    else do
      phi <-
        IRB.phi [(fromJust then'', thenLabel), (fromJust else'', elseLabel)]
      return $ Just phi
genExpression (T.ELet (C.Annoted name ty) expr) = do
  ty' <- fromType ty
  Just expr' <- genExpression expr
  expr'' <-
    AST.typeOf expr' >>= \(Right e) ->
      if ty' /= e
        then bitcast expr' ty'
        else return expr'
  i <- IRB.alloca ty' Nothing 0
  IRB.store i 0 expr''
  ST.modify $ \s -> s {lsEnv = M.insert name i (lsEnv s)}
  return Nothing
genExpression (T.EProperty expr field) = do
  expr' <- genExpression expr
  case expr' of
    Just expr'' -> do
      ty <- AST.typeOf expr''
      case ty of
        Left err -> error $ "genExpression: " <> show err
        Right (AST.NamedTypeReference (AST.Name name')) -> do
          struct <- ST.gets (M.lookup (decodeUtf8 name') . lsAliases)
          case struct of
            Just (struct', _) -> do
              props <- ST.gets (M.lookup struct' . lsStructs)
              case props of
                Just props' -> do
                  case M.lookup field props' of
                    Just i -> Just <$> IRB.extractValue expr'' [fromIntegral i]
                    Nothing ->
                      error $ "genExpression: field " <> field <> " not found"
                Nothing ->
                  error $
                  "genExpression: struct " <> decodeUtf8 name' <> " not found"
            Nothing ->
              error $
              "genExpression: struct " <> decodeUtf8 name' <> " not found"
        _ -> error $ "genExpression: expr' is not a struct: " <> show expr
    _ -> error "genExpression: expr' is Nothing"
genExpression (T.EStruct t fields) = do
  struct' <- fromType $ T.TId t
  fields' <- mapM (genExpression . C.annotedType) fields
  let fields'' = map fromJust fields'
  v <- IRB.alloca struct' Nothing 0
  forM_ (zip [0 ..] fields'') $ \(i, field) -> do
    field' <- IRB.gep v [IRB.int32 0, IRB.int32 i]
    IRB.store field' 0 field
  Just <$> IRB.load v 0
genExpression (T.EList t elems) = do
  elems' <- mapM genExpression elems
  let elems'' = map fromJust elems'
  ty <- fromType t
  var <- IRB.alloca ty Nothing 0
  forM_ (zip [0 ..] elems'') $ \(i, elem') -> do
    elem'' <- IRB.gep var [IRB.int32 i]
    IRB.store elem'' 0 elem'
  return $ Just var
genExpression (T.EIndex expr index') = do
  expr' <- fromJust <$> genExpression expr
  index'' <- fromJust <$> genExpression index'
  i <- IRB.gep expr' [index'']
  Just <$> IRB.load i 0
genExpression (T.EUpdate update expr) = do
  update' <- fromJust <$> genUpdate update
  expr' <- fromJust <$> genExpression expr
  upTy <- AST.typeOf update'
  exprTy <- AST.typeOf expr'
  expr'' <-
    case exprTy of
      Right ty ->
        case upTy of
          Right (AST.PointerType x _)
            | x == ty -> return expr'
            | otherwise -> bitcast expr' x
          _ -> error $ "genExpression: " <> show upTy <> " /= " <> show exprTy
      Left err -> error $ "genExpression: " <> show err
  IRB.store update' 0 expr''
  return Nothing
genExpression (T.EWhile cond body) = do
  condLabel <- IRB.freshName "cond"
  bodyLabel <- IRB.freshName "body"
  mergeLabel <- IRB.freshName "merge"
  IRB.br condLabel
  namedBlock condLabel
  cond' <- genExpression cond
  IRB.condBr (fromJust cond') bodyLabel mergeLabel
  namedBlock bodyLabel
  viaNonEmpty last . catMaybes <$> mapM genExpression body
  IRB.br condLabel
  namedBlock mergeLabel
  return Nothing
genExpression (T.EFor name from to body) = do
  from' <- fromJust <$> genExpression from
  to' <- fromJust <$> genExpression to
  i <- IRB.alloca AST.i32 Nothing 0
  ST.modify $ \s -> s {lsEnv = M.insert name i (lsEnv s)}
  IRB.store i 0 from'
  condLabel <- IRB.freshName "cond"
  bodyLabel <- IRB.freshName "body"
  mergeLabel <- IRB.freshName "merge"
  IRB.br condLabel
  namedBlock condLabel
  i' <- IRB.load i 0
  cond' <- IRB.icmp IP.SLT i' to'
  IRB.condBr cond' bodyLabel mergeLabel
  namedBlock bodyLabel
  mapM_ genExpression body
  i'' <- IRB.load i 0
  i''' <- IRB.add i'' (IRB.int32 1)
  IRB.store i 0 i'''
  IRB.br condLabel
  namedBlock mergeLabel
  ST.modify $ \s -> s {lsEnv = M.delete name (lsEnv s)}
  return Nothing
genExpression (T.EDereference expr) = do
  expr' <- fromJust <$> genExpression expr
  Just <$> IRB.load expr' 0
genExpression (T.EReference expr) = do
  expr' <- fromJust <$> genExpression expr
  ty <- AST.typeOf expr'
  case ty of
    Left err -> error $ fromString err
    Right ty' -> do
      name <- IRB.alloca ty' Nothing 0
      IRB.store name 0 expr'
      return $ Just name
genExpression (T.ESizeOf t) = do
  t' <- fromType t
  Just <$> IRB.sizeof 32 t'
genExpression (T.ELiteral l) =
  case l of
    L.Int i -> return $ Just $ IRB.int32 i
    L.Float f -> return $ Just $ IRB.double f
    L.Bool b ->
      return $
      Just $
      AST.ConstantOperand $
      AST.Int 1 $
      if b
        then 1
        else 0
    L.Char c -> return $ Just $ IRB.int8 $ fromIntegral $ ord c
    L.String s -> do
      s' <- string s
      return (Just s')
genExpression z@T.EAssembly {} = parseAssembly z

genUpdate :: LLVM m => T.UpdateExpression -> m (Maybe AST.Operand)
genUpdate (T.UVariable name) = do
  env <- ST.gets lsEnv
  case M.lookup name env of
    Just op -> return (Just op)
    Nothing -> error $ "genExpression: variable " <> name <> " not found"
genUpdate (T.UProperty expr field) = do
  expr' <- fromJust <$> genUpdate expr
  ty <- AST.typeOf expr'
  case ty of
    Left err -> error $ "genUpdate: " <> show err
    Right (AST.PointerType (AST.NamedTypeReference (AST.Name name')) _) -> do
      struct <- ST.gets (M.lookup (decodeUtf8 name') . lsAliases)
      case struct of
        Just (struct', _) -> do
          props <- ST.gets (M.lookup struct' . lsStructs)
          case props of
            Just props' -> do
              case M.lookup field props' of
                Just i ->
                  Just <$> IRB.gep expr' [AST.int32 0, AST.int32 (toInteger i)]
                Nothing -> error $ "genUpdate: field " <> field <> " not found"
            Nothing ->
              error $ "genUpdate: struct " <> decodeUtf8 name' <> " not found"
        Nothing ->
          error $ "genUpdate: struct " <> decodeUtf8 name' <> " not found"
    Right _ -> error "genUpdate: not implemented"
genUpdate (T.UIndex expr index') = do
  expr' <- fromJust <$> genUpdate expr
  index'' <- fromJust <$> genExpression index'
  expr'' <- IRB.load expr' 0
  Just <$> IRB.gep expr'' [index'']
genUpdate (T.UDereference expr) = do
  expr' <- fromJust <$> genUpdate expr
  Just <$> IRB.load expr' 0

parseAssembly :: LLVM m => T.Expression -> m (Maybe AST.Operand)
parseAssembly (T.EAssembly "gep" (x:ys)) = do
  x' <- fromJust <$> genExpression x
  y' <- fromJust . sequence <$> mapM genExpression ys
  Just <$> IRB.gep x' y'
parseAssembly (T.EAssembly "load" (x:_)) = do
  x' <- fromJust <$> genExpression x
  Just <$> IRB.load x' 0
parseAssembly (T.EAssembly "mul" (x:y:_)) = do
  x' <- fromJust <$> genExpression x
  y' <- fromJust <$> genExpression y
  Just <$> IRB.mul x' y'
parseAssembly (T.EAssembly "sdiv" (x:y:_)) = do
  x' <- fromJust <$> genExpression x
  y' <- fromJust <$> genExpression y
  Just <$> IRB.sdiv x' y'
parseAssembly (T.EAssembly "add" (x:y:_)) = do
  x' <- fromJust <$> genExpression x
  y' <- fromJust <$> genExpression y
  Just <$> IRB.add x' y'
parseAssembly (T.EAssembly "sub" (x:y:_)) = do
  x' <- fromJust <$> genExpression x
  y' <- fromJust <$> genExpression y
  Just <$> IRB.sub x' y'
parseAssembly (T.EAssembly "icmp" (T.ELiteral (L.String cmp):x:y:_)) = do
  x' <- fromJust <$> genExpression x
  y' <- fromJust <$> genExpression y
  let cmp' =
        case cmp of
          "eq"  -> IP.EQ
          "ne"  -> IP.NE
          "sgt" -> IP.SGT
          "sge" -> IP.SGE
          "slt" -> IP.SLT
          "sle" -> IP.SLE
          "ugt" -> IP.UGT
          "uge" -> IP.UGE
          "ult" -> IP.ULT
          "ule" -> IP.ULE
          _     -> error "parseAssembly: invalid icmp"
  Just <$> IRB.icmp cmp' x' y'
parseAssembly (T.EAssembly "fcmp" (T.ELiteral (L.String cmp):x:y:_)) = do
  x' <- fromJust <$> genExpression x
  y' <- fromJust <$> genExpression y
  let cmp' =
        case cmp of
          "false" -> FP.False
          "oeq"   -> FP.OEQ
          "ogt"   -> FP.OGT
          "oge"   -> FP.OGE
          "olt"   -> FP.OLT
          "ole"   -> FP.OLE
          "one"   -> FP.ONE
          "ord"   -> FP.ORD
          "ueq"   -> FP.UEQ
          "ugt"   -> FP.UGT
          "uge"   -> FP.UGE
          "ult"   -> FP.ULT
          "ule"   -> FP.ULE
          "une"   -> FP.UNE
          "uno"   -> FP.UNO
          "true"  -> FP.True
          _       -> error "parseAssembly: invalid fcmp"
  Just <$> IRB.fcmp cmp' x' y'
parseAssembly z = error $ "parseAssembly: not implemented: " <> show z

bitcastStruct :: LLVM m => AST.Operand -> AST.Type -> m AST.Operand
bitcastStruct op ty = do
  ty' <- AST.typeOf op
  case ty' of
    Left err -> error $ "bitcastStruct: " <> show err
    Right ty'' -> do
      propsOperand <- getFields ty''
      propsTy <- getFields ty
      let props = M.toList propsOperand
      let props' = M.toList propsTy
      let props'' = assoc props props'
      v <- IRB.alloca ty Nothing 0
      forM_ props'' $ \(_, ((_, i1), (ty2, i2))) -> do
        field <- IRB.extractValue op [fromIntegral i1]
        field' <- IRB.gep v [IRB.int32 0, IRB.int32 (fromIntegral i2)]
        valueBitcasted <- bitcast field ty2
        IRB.store field' 0 valueBitcasted
      return v

bitcast :: LLVM m => AST.Operand -> AST.Type -> m AST.Operand
bitcast op ty = do
  ty' <- AST.typeOf op
  case (ty', ty) of
    (Left err, _) -> error $ "bitcast: " <> show err
    (Right ty'', ty''')  -> do
      b1 <- isStruct ty''
      b2 <- isStruct ty'''
      if b1 && b2
        then bitcastStruct op ty
        else IRB.bitcast op ty

isStruct :: LLVM m => AST.Type -> m Bool
isStruct ty = do
  case ty of
    AST.NamedTypeReference (AST.Name name) -> do
      struct <- ST.gets (M.lookup (decodeUtf8 name) . lsAliases)
      case struct of
        Just _ -> return True
        _ -> return False
    AST.StructureType False _ -> return True
    _ -> return False

assoc :: Eq a => [(a, b)] -> [(a, c)] -> [(a, (b, c))]
assoc [] _ = []
assoc ((x, y):xs) ys =
  case L.lookup x ys of
    Just z -> (x, (y, z)) : assoc xs ys
    Nothing -> assoc xs ys

getFields :: LLVM m => AST.Type -> m (M.Map Text (AST.Type, Int))
getFields ty = do
  case ty of
    AST.NamedTypeReference (AST.Name name) -> do
      struct <- ST.gets (M.lookup (decodeUtf8 name) . lsAliases)
      case struct of
        Just (struct', AST.StructureType False fieldsTy) -> do
          props <- ST.gets (M.lookup struct' . lsStructs)
          case props of
            Just props' -> return . M.fromList $ zipWith (\t (n, i) -> (n, (t, i))) fieldsTy (M.toList props')
            Nothing ->
              error $ "getFields: struct " <> decodeUtf8 name <> " not found"
        _ ->
          error $ "getFields: struct " <> decodeUtf8 name <> " not found"
    _ -> error "getFields: not implemented"