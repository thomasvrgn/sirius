{-# LANGUAGE LambdaCase #-}
module Language.Sirius.LLVM where
import qualified Control.Monad.State as ST
import qualified LLVM.IRBuilder as IRB
import qualified Data.Map as M
import qualified LLVM.AST as AST
import qualified LLVM.Context as LL
import qualified LLVM.Module as LL
import qualified LLVM.Target as LL
import Language.Sirius.LLVM.Modules.Monad (LLVMState(LLVMState))
import Language.Sirius.LLVM.Codegen (declare, genToplevel)
import qualified System.Directory as IO
import qualified System.Process as IO
import System.FilePath
import qualified Language.Sirius.ANF.AST as ANF

findCompiler :: [String] -> IO (Maybe String)
findCompiler [] = return Nothing
findCompiler (x:xs) = do
  IO.findExecutable x >>= \case
    Just command -> return $ Just command
    Nothing -> findCompiler xs

getCompiler :: IO (Maybe String)
getCompiler = findCompiler $ "gcc" : "clang" : [ "clang-" ++ show i | i <- [(9 :: Int)..16]]

runLLVM :: MonadFail m => [ANF.Toplevel] -> m AST.Module
runLLVM xs =
  ST.evalStateT
    (IRB.buildModuleT "main" $
     IRB.runIRBuilderT IRB.emptyIRBuilder (declare xs *> mapM genToplevel xs))
    (LLVMState 0 M.empty M.empty M.empty)

getLLContent :: [ANF.Toplevel] -> IO ByteString
getLLContent xs = do
  mod' <- runLLVM xs
  LL.withContext $ \context ->
    LL.withModuleFromAST context mod' $ \mod''' -> 
      LL.moduleLLVMAssembly mod'''

runLLVMPass :: [ANF.Toplevel] -> IO Text
runLLVMPass xs = do
  mod' <- runLLVM xs
  s <-
    LL.withContext $ \context ->
      LL.withModuleFromAST context mod' $ \mod''' -> 
        LL.withHostTargetMachineDefault $ \tm -> do
    LL.moduleTargetAssembly tm mod'''
  return $ decodeUtf8 s

pathToLib :: FilePath
pathToLib = "src/Language/Sirius/LLVM/"

runCompilerPass :: [ANF.Toplevel] -> IO ()
runCompilerPass xs = do
  compiler <- getCompiler
  case compiler of
    Just compiler' -> do
      content <- runLLVMPass xs
      writeFileText "main.s" content
      IO.callProcess compiler' ["-o", "main", "main.s", pathToLib </> "lib.c"]
    Nothing -> error "No compiler found"
  