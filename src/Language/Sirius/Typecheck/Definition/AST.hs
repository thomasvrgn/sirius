module Language.Sirius.Typecheck.Definition.AST where

import           Language.Sirius.CST.Modules.Annoted
import           Language.Sirius.CST.Modules.Literal
import           Language.Sirius.Typecheck.Definition.Type
import           Prelude                                   hiding (Generic,
                                                            Type)
import qualified Text.Show as T
import qualified Data.List as L
import Language.Sirius.CST.Modules.Located (Position)

type Property = Annoted Type

-- ^ A property is a type annotation that is used to
--   specify on which type a function can be called.
type Generic = Type

-- ^ A generic type variable is a type variable that
--   can be used in a type annotation.
type Name = Text

type Argument = Annoted Type

type Field = Annoted Type

-- ^ A field is a type annotation that is used to
--   specify the type of a field in a struct.
type Path = [Text]

-- ^ A path is a list of names that is used to
--   specify the location of a module.
--   e.g: std::io::println is represented as
--   ["std", "io", "println"]
data Toplevel
  = TFunction [Generic] (Annoted Type) [Argument] Expression
  -- ^ TFunction denotes a function declaration
  --   The first argument is a list of generic type variables
  --   The second argument is the return type
  --   The third argument is a list of arguments
  --   The fourth argument is the body of the function
  | TExtern [Generic] (Annoted Type)
  -- ^ TExtern denotes an extern function declaration
  --   The first argument is a list of generic type variables
  --   The second argument is the return type
  | TStruct (Annoted [Generic]) [Field]
  -- ^ TStruct denotes a struct declaration
  --   The first argument is a list of generic type variables
  --   The second argument is a list of fields
  | TFunctionProp [Generic] Property (Annoted Type) [Argument] Expression
  -- ^ TFunctionProp denotes a function declaration with a property
  --   The first argument is a list of generic type variables
  --   The second argument is the property
  --   The third argument is the return type
  --   The fourth argument is a list of arguments
  --   The fifth argument is the body of the function
  | TEnumeration (Annoted [Generic]) [Annoted Type]
  -- ^ TEnumeration denotes an enumeration declaration
  --   The first argument is a list of generic type variables
  --   The second argument is a list of types
  | TUnion Text [Annoted Type]
  deriving (Eq)

data Expression
  = EVariable Name Type
  -- ^ EVariable denotes a variable
  --   The argument is the name of the variable
  | ELocated Expression Position
  -- ^ Located denotes an expression with a position
  | EClassVariable Name Type Type
  -- ^ EClassVariable denotes a class variable
  --   The argument is the name of the variable
  --   The second argument is the type of the class
  --   The third argument is the type of the variable
  | EApplication Expression [Expression] Type
  -- ^ EApplication denotes a function application
  --   The first argument is the function
  --   The second argument is a list of arguments
  | ELet (Annoted Type) Expression (Maybe Expression) (Maybe Type)
  -- ^ ELet denotes a let binding
  --   The first argument is the name of the variable
  --   The second argument is the value of the variable
  --   The third argument is the body of the let binding
  | EIf Expression [Expression] [Expression]
  -- ^ EIf denotes an if expression
  --   The first argument is the condition
  --   The second argument is the then branch
  --   The third argument is the else branch
  | ELiteral Literal
  -- ^ ELiteral denotes a literal
  --   The argument is the literal
  | EProperty Expression Name
  -- ^ EProperty denotes a property access
  --   The first argument is the object
  --   The second argument is the name of the property
  | EFunction Type [Argument] Expression
  -- ^ EFunction denotes a function
  --   The first argument is a list of arguments
  --   The second argument is the body of the function
  | EStruct Type [Annoted Expression]
  -- ^ EStruct denotes a struct
  --   The first argument is a list of generic type variables
  --   The second argument is a list of fields
  | EList [Expression] Type
  -- ^ EList denotes a list
  --   The argument is a list of elements
  | EIndex Expression Expression
  -- ^ EIndex denotes an index access
  --   The first argument is the object
  | EBlock [Expression]
  -- ^ EBlock denotes a block
  --   The argument is a list of expressions
  | EUpdate UpdateExpression Expression
  -- ^ EUpdate denotes an update expression
  --   The first argument is the update expression
  --   The second argument is the object
  | EDereference Expression
  -- ^ EDereference denotes a dereference expression
  --   The argument is the object
  | EReference Expression
  -- ^ EReference denotes a reference expression
  --   The argument is the object
  | EFor (Annoted Type) Expression Expression [Expression]
  -- ^ EFor denotes a for loop
  --   The first argument is the name of the variable
  --   The second argument is the iterable start value
  --   The third argument is the iterable end value
  --   The fourth argument is the body of the for loop
  | EWhile Expression [Expression]
  -- ^ EWhile denotes a while loop
  --   The first argument is the condition
  --   The second argument is the body of the while loop
  | ESizeOf Type
  -- ^ ESizeOf denotes a sizeof expression
  --   The argument is the type
  | EAssembly Text [Expression]
  -- ^ EAssembly denotes an assembly expression
  --   The first argument is the assembly code
  --   The second argument is a list of arguments
  | EDeclaration Text Type
  -- ^ EDeclaration denotes a declaration
  --   The first argument is the name of the declaration
  --   The second argument is the type of the declaration
  | EInternalField Expression Int
  -- ^ EInternalField denotes an internal field access
  --   The first argument is the object
  --   The second argument is the index of the field
  | EMatch Expression [MatchCase]
  deriving (Eq)

type MatchCase = (Pattern, Expression)

data Pattern
  = PVariable Name Type
  -- ^ PVariable denotes a variable
  --   The argument is the name of the variable
  | PStruct Type [Annoted Pattern]
  -- ^ PStruct denotes a struct
  --   The first argument is a list of generic type variables
  --   The second argument is a list of fields
  | PApp Name [Pattern] Type
  -- ^ PApp denotes a pattern application
  --   The first argument is the name of the function
  --   The second argument is a list of arguments
  | PWildcard 
  -- ^ PWildcard denotes a wildcard
  | PLiteral Literal
  -- ^ PLiteral denotes a literal
  --   The argument is the literal
  deriving Eq

data UpdateExpression
  = UVariable Name Type
  -- ^ UVariable denotes a variable
  --   The argument is the name of the variable
  | UProperty UpdateExpression Name
  -- ^ UProperty denotes a property access
  --   The first argument is the object
  --   The second argument is the name of the property
  | UIndex UpdateExpression Expression
  -- ^ UIndex denotes an index access
  --   The first argument is the object
  --   The second argument is the index
  | UDereference UpdateExpression
  | UInternalField UpdateExpression Int
  deriving (Eq)

instance T.Show UpdateExpression where
  show (UVariable name _) = toString name
  show (UProperty object name) = T.show object ++ "." ++ T.show name
  show (UIndex object index) = T.show object ++ "[" ++ T.show index ++ "]"
  show (UDereference object) = "*" ++ T.show object
  show (UInternalField object index) =
    T.show object ++ "." ++ show index

instance T.Show Toplevel where
  show (TFunction generics returnType arguments body) =
    "fn " ++
    T.show generics ++
    " " ++
    T.show returnType ++
    " " ++
    T.show arguments ++ " => " ++ T.show body
  show (TExtern generics returnType) =
    "extern fn " ++ T.show generics ++ " " ++ T.show returnType
  show (TStruct generics fields) =
    "struct " ++ T.show generics ++ " {" ++ T.show fields ++ "}"
  show (TFunctionProp generics property returnType arguments body) =
    "fn " ++
    T.show generics ++
    " " ++
    T.show property ++
    " " ++
    T.show returnType ++
    " " ++
    T.show arguments ++ " => " ++ T.show body
  show (TEnumeration generics variants) =
    "enum " ++ T.show generics ++ " {" ++ T.show variants ++ "}"
  show (TUnion generics variants) =
    "union " ++ T.show generics ++ " {" ++ T.show variants ++ "}"

instance T.Show Expression where
  show (EVariable name t) = "(" <> toString name <> ": " <> show t <> ")"
  show (EClassVariable name _ app) = "(" <> toString name <> ": " <> show app <> ")"
  show (EApplication function arguments _) =
    T.show function ++ "(" ++ L.intercalate ", " (map T.show arguments) ++ ")"
  show (ELet name value body _) =
    "let " ++ T.show name ++ " = " ++ T.show value ++ " in " ++ T.show body
  show (EIf condition thenBranch elseBranch) =
    "if " ++
    T.show condition ++
    " then " ++ T.show thenBranch ++ " else " ++ T.show elseBranch
  show (ELiteral literal) = T.show literal
  show (EProperty object name) = T.show object ++ "." ++ toString name
  show (EFunction _ arguments body) =
    "fn " ++ T.show arguments ++ " => " ++ T.show body
  show (EStruct n fields) = T.show n ++ " {" ++ T.show fields ++ "}"
  show (EList elements _) = "[" ++ T.show elements ++ "]"
  show (EIndex object index) = T.show object ++ "[" ++ T.show index ++ "]"
  show (EBlock expressions) = "{" ++ L.unwords (map T.show expressions) ++ "}"
  show (EUpdate updateExpression object) =
    T.show updateExpression ++ " " ++ T.show object
  show (EDereference object) = "*" ++ T.show object
  show (EReference object) = "&" ++ T.show object
  show (EFor name start end body) =
    "for " ++
    T.show name ++
    " in " ++
    T.show start ++ ".." ++ T.show end ++ " " ++ T.show body
  show (EWhile condition body) =
    "while " ++ T.show condition ++ " " ++ T.show body
  show (ESizeOf t) = "sizeof(" ++ T.show t ++ ")"
  show (ELocated expression _) = T.show expression
  show (EAssembly code arguments) =
    "asm(" ++ toString code ++ ", " ++ T.show arguments ++ ")"
  show (EDeclaration name t) = "declare " ++ toString name ++ ": " ++ T.show t
  show (EInternalField object index) =
    T.show object ++ "." ++ show index
  show (EMatch expression cases) =
    "match " ++
    T.show expression ++
    " {" ++ L.unwords (map showCase cases) ++ "}"
    where
      showCase (pattern, expression') =
        T.show pattern ++ " => " ++ T.show expression'
        
instance T.Show Pattern where
  show (PVariable name t) = "(" <> toString name <> ": " <> show t <> ")"
  show (PStruct t fields) = T.show t ++ " {" ++ T.show fields ++ "}"
  show (PApp name arguments _) =
    T.show name ++ "(" ++ L.intercalate ", " (map T.show arguments) ++ ")"
  show PWildcard = "_"
  show (PLiteral literal) = T.show literal