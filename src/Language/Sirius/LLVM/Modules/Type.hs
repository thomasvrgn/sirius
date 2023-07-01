{-# LANGUAGE FlexibleContexts #-}
module Language.Sirius.LLVM.Modules.Type where
import Language.Sirius.LLVM.Modules.Monad (LLVM, LLVMState (lsAliases, lsStructs))
import qualified Language.Sirius.Typecheck.Definition.Type as T
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST as AST
import qualified Control.Monad.State as ST
import qualified Data.Map as M

toBS :: Text -> ShortByteString
toBS = fromString . toString

toText :: ShortByteString -> Text
toText = fromString . decodeUtf8

fromType :: LLVM m => T.Type -> m AST.Type
fromType T.Int = return AST.i32
fromType T.Float = return AST.double
fromType T.Void = return AST.i32
fromType T.Bool = return AST.i1
fromType T.TString = return $ AST.ptr AST.i8
fromType T.Char = return AST.i8
fromType (T.TAddr T.Void) = return $ AST.ptr AST.i8
fromType (T.TList T.Void) = return $ AST.ptr AST.i8
fromType (T.TAddr (T.TVar _)) = return $ AST.ptr AST.i8
fromType (T.TAddr t) = AST.ptr <$> fromType t
fromType (T.TVar _) = return $ AST.ptr AST.i8
fromType (args T.:-> ret) = do
  args' <- mapM fromType args
  ret' <- fromType ret
  return $ AST.ptr $ AST.FunctionType ret' args' False
fromType (T.TApp (T.TId "Union") [T.TId env1, T.TId env2]) = do
  alias1 <- ST.gets (M.lookup env1 . lsAliases)
  alias2 <- ST.gets (M.lookup env2 . lsAliases)

  case (alias1, alias2) of
    (Just (a1, _), Just (a2, _)) -> do
      struct1 <- ST.gets (M.lookup a1 . lsStructs)
      struct2 <- ST.gets (M.lookup a2 . lsStructs)
      return $ if length struct1 > length struct2 
        then a1
        else a2
    _ -> error $ "fromType: union " <> env1 <> " " <> env2 <> " not found"
fromType (T.TId name) = do
  alias <- ST.gets (M.lookup name . lsAliases)
  case alias of
    Just (alias', _) -> return alias'
    Nothing -> error $ "fromType: alias " <> name <> " not found"
fromType (T.TRec name _) = return $ AST.NamedTypeReference (AST.Name $ toBS name)
fromType (T.TList t) = AST.ptr <$> fromType t
fromType z@(T.TApp _ _) = error $ "fromType: encountered TApp: " <> show z