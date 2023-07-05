{-# LANGUAGE FlexibleContexts #-}
module Language.Sirius.LLVM.Modules.Type where
import Language.Sirius.LLVM.Modules.Monad (LLVM, LLVMState (lsAliases, lsStructs))
import qualified Language.Sirius.Typecheck.Definition.Type as T
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST as AST
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Language.Sirius.ANF.AST as T
import qualified Language.Sirius.CST.Modules.Annoted as C
import LLVM.AST (Type(StructureType))
import LLVM.IRBuilder (typedef)

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
fromType (T.TId name) = do
  alias <- ST.gets (M.lookup name . lsAliases)
  case alias of
    Just (alias', _) -> return alias'
    Nothing -> error $ "fromType: alias " <> name <> " not found"
fromType (T.TRec name f) = do
  alias <- ST.gets (M.lookup name . lsAliases)
  case alias of
    Just (alias', _) -> return alias'
    Nothing -> do
      toDebrujinStruct $ T.TStruct name (map (uncurry C.Annoted) f)
      alias' <- ST.gets (M.lookup name . lsAliases)
      case alias' of
        Just (alias'', _) -> return alias''
        Nothing -> error $ "fromType: alias " <> name <> " not found"
fromType (T.TList t) = AST.ptr <$> fromType t
fromType z@(T.TApp _ _) = error $ "fromType: encountered TApp: " <> show z

toDebrujinStruct :: LLVM m => T.Toplevel -> m ()
toDebrujinStruct (T.TStruct name fields) = do
  fieldsTy <- mapM (fromType . C.annotedType) fields
  ty <- typedef (AST.Name $ toBS name) $ Just $ StructureType False fieldsTy
  ST.modify $ \s -> s {lsAliases = M.insert name (ty, StructureType False fieldsTy) (lsAliases s)}
  ST.modify $ \s ->
    s
      { lsStructs =
          M.insert
            ty
            (M.fromList $ zip (map C.annotedName fields) [0 ..])
            (lsStructs s)
      }
toDebrujinStruct _ = return ()
