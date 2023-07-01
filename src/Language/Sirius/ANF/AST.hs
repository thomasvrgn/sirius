module Language.Sirius.ANF.AST where

import           Language.Sirius.CST.Modules.Annoted       (Annoted)
import           Language.Sirius.CST.Modules.Literal       (Literal)
import           Language.Sirius.Typecheck.Definition.Type
import           Prelude                                   hiding (Type)
import qualified Language.Sirius.CST.Modules.Annoted as C

data Expression
  = ELiteral Literal
  | EVariable Text
  | EApplication Text [Expression]
  | EIf Expression [Expression] [Expression]
  | EProperty Expression Text
  | EStruct Text [Annoted Expression]
  | EList Type [Expression]
  | EIndex Expression Expression
  | EDereference Expression
  | EReference Expression
  | ESizeOf Type
  | EAssembly Text [Expression]
  | EBlock [Expression]
  | EWhile Expression [Expression]
  | EFor Text Expression Expression [Expression]
  | ELet (C.Annoted Type) Expression
  | EUpdate UpdateExpression Expression
  deriving Show

data Toplevel
  = TFunction (Annoted Type) [Annoted Type] Expression
  | TExtern (Annoted Type)
  | TStruct Text [Annoted Type]
  deriving Show

data UpdateExpression 
  = UVariable Text
  | UProperty UpdateExpression Text
  | UIndex UpdateExpression Expression
  | UDereference UpdateExpression
  deriving Show