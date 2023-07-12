{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Sirius.LLVM.Modules.Monad where

import qualified Control.Monad.RWS as ST
import qualified Data.Map          as M
import qualified LLVM.AST          as AST
import qualified LLVM.IRBuilder    as IRB
import qualified Language.Sirius.Typecheck.Definition.Type as T

type LLVM m
   = (IRB.MonadModuleBuilder m, ST.MonadState LLVMState m, IRB.MonadIRBuilder m, MonadFail m)

data LLVMState =
  LLVMState
    { lsCounter :: Int
    , lsEnv     :: M.Map Text AST.Operand
    , lsStructs :: M.Map AST.Type (M.Map Text Int)
    , lsAliases :: M.Map Text (AST.Type, AST.Type)
    , lsUnions  :: M.Map AST.Type (M.Map T.Type AST.Type)
    }
  deriving (Show)


fresh :: LLVM m => m Int
fresh = do
  i <- ST.gets lsCounter
  ST.modify $ \s -> s {lsCounter = i + 1}
  return i
