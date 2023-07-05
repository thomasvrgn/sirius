{-# LANGUAGE FlexibleContexts #-}

module Language.Sirius.LLVM.Modules.UnionDebrujin where

import qualified Control.Monad.State                      as ST
import qualified Data.Map                                 as M
import qualified Language.Sirius.CST.Modules.Annoted      as C
import           Language.Sirius.LLVM.Modules.Monad       (LLVM,
                                                           LLVMState (lsAliases, lsStructs))
import           Language.Sirius.LLVM.Modules.Type        (fromType, toBS)
import qualified Language.Sirius.ANF.AST as T
import           LLVM.AST                                 (Type (StructureType))
import qualified LLVM.AST                                 as AST
import           LLVM.IRBuilder                           (typedef)

toDebrujinUnion :: LLVM m => T.Toplevel -> m ()
toDebrujinUnion (T.TUnion name fields) = do
  ty <- typedef (AST.Name $ toBS name) Nothing
  ST.modify $ \s -> s {lsAliases = M.insert name (ty, StructureType False []) (lsAliases s)}
  fieldsTy <- mapM (fromType . C.annotedType) fields
  ty' <- typedef (AST.Name $ toBS $ name) $ Just $ StructureType False fieldsTy
  ST.modify $ \s -> s {lsAliases = M.insert name (ty', StructureType False fieldsTy) (lsAliases s)}
  ST.modify $ \s ->
    s
      { lsStructs =
          M.insert
            ty
            (M.fromList $ zip (map C.annotedName fields) [0 ..])
            (lsStructs s)
      }
toDebrujinUnion _ = return ()
