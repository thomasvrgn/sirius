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
import qualified Data.List as L
import qualified Data.Maybe as MB
import qualified Data.Text as T

-- Invalid sizeof but let the compiler know which 
-- types are the heaviest
sizeOf :: LLVM m => AST.Type -> m Int
sizeOf (AST.IntegerType _) = return 4
sizeOf (AST.PointerType s _) = (+8) <$> sizeOf s
sizeOf (AST.StructureType _ xs) = sum <$> mapM sizeOf xs
sizeOf (AST.ArrayType _ x) = sizeOf x
sizeOf (AST.FloatingPointType _) = return 8
sizeOf (AST.VectorType _ x) = sizeOf x
sizeOf AST.MetadataType = return 8
sizeOf AST.LabelType = return 8
sizeOf AST.VoidType = return 0
sizeOf (AST.FunctionType ret args _) = do
  retSize <- sizeOf ret
  argSizes <- mapM sizeOf args
  return $ retSize + sum argSizes
sizeOf AST.TokenType = return 8
sizeOf (AST.NamedTypeReference (AST.Name x)) = do
  aliases <- ST.gets lsAliases
  case M.lookup (decodeUtf8 x) aliases of
    Just (_, ty) -> sizeOf ty
    Nothing -> error $ "Type " <> show x <> " not found"
sizeOf (AST.NamedTypeReference _) = return 8

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
fromType (T.TApp (T.TId n) tys) | "$$Either" `T.isPrefixOf` n = do
  tys' <- mapM fromType tys
  sizes <- mapM sizeOf tys'
  let max' = L.maximum sizes
  let fieldTypeMax = find (\(_, size) -> size == max') $ zip tys' sizes
  return $ fst $ MB.fromJust fieldTypeMax
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
