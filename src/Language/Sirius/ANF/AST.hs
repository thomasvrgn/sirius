module Language.Sirius.ANF.AST where

import           Language.Sirius.CST.Modules.Annoted       (Annoted)
import           Language.Sirius.CST.Modules.Literal       (Literal)
import           Language.Sirius.Typecheck.Definition.Type
import           Prelude                                   hiding (Type)
import qualified Language.Sirius.CST.Modules.Annoted as C
import qualified Text.Show as T

data Expression
  = ELiteral Literal
  | EVariable Text
  | EApplication Text [Expression]
  | EIfElse Expression [Expression] [Expression] Type
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
  | EDeclaration Text Type
  | EInternalField Expression Int
  deriving Eq

instance T.Show Expression where
  show (ELiteral l) = T.show l
  show (EVariable v) = toString v
  show (EApplication name args) = toString name <> "(" <> T.show args <> ")"
  show (EIfElse cond then' else' _) = "if " <> T.show cond <> " then " <> T.show then' <> " else " <> T.show else'
  show (EProperty e name) = T.show e <> "." <> toString name
  show (EStruct name fields) = toString name <> "{" <> T.show fields <> "}"
  show (EList ty fields) = "[" <> T.show ty <> "]" <> T.show fields
  show (EIndex e1 e2) = T.show e1 <> "[" <> T.show e2 <> "]"
  show (EDereference e) = "*" <> T.show e
  show (EReference e) = "&" <> T.show e
  show (ESizeOf ty) = "sizeof(" <> T.show ty <> ")"
  show (EAssembly name args) = "asm " <> toString name <> "(" <> T.show args <> ")"
  show (EBlock exprs) = "{" <>  concatMap ((<> "\n") . ("  " <>) . T.show) exprs <> "}"
  show (EWhile cond body) = "while " <> T.show cond <> " " <> T.show body
  show (EFor name from to body) = "for " <> toString name <> " in " <> T.show from <> ".." <> T.show to <> " " <> T.show body
  show (ELet ty expr) = "let " <> T.show ty <> " = " <> T.show expr
  show (EUpdate e1 e2) = T.show e1 <> " = " <> T.show e2
  show (EDeclaration name ty) = T.show ty <> " " <> toString name
  show (EInternalField e i) = T.show e <> "." <> show i

data Toplevel
  = TFunction (Annoted Type) [Annoted Type] Expression
  | TExtern (Annoted Type)
  | TStruct Text [Annoted Type]
  | TUnion Text [C.Annoted Type]
  deriving (Eq)

instance T.Show Toplevel where
  show (TFunction name args body) = "fun " <> T.show name <> " (" <> intercalate "," (map T.show args) <> ") = " <> T.show body
  show (TExtern name) = "extern " <> T.show name
  show (TStruct name fields) = "struct " <> toString name <> " " <> T.show fields
  show (TUnion name fields) = "union " <> toString name <> " " <> T.show fields

data UpdateExpression 
  = UVariable Text
  | UProperty UpdateExpression Text
  | UIndex UpdateExpression Expression
  | UDereference UpdateExpression
  | UInternalField UpdateExpression Int
  deriving Eq

instance T.Show UpdateExpression where
  show (UVariable v) = toString v
  show (UProperty e name) = T.show e <> "." <> toString name
  show (UIndex e1 e2) = T.show e1 <> "[" <> T.show e2 <> "]"
  show (UDereference e) = "*" <> T.show e
  show (UInternalField e i) = T.show e <> "." <> show i