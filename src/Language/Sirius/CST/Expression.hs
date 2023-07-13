module Language.Sirius.CST.Expression where

import           Prelude                             hiding (Generic, Type)

import           Language.Sirius.CST.Modules.Annoted
import           Language.Sirius.CST.Modules.Literal
import           Language.Sirius.CST.Modules.Located
import           Language.Sirius.CST.Modules.Type
import Language.Sirius.CST.Modules.Namespaced (Namespaced)

type Property = Annoted Type

-- ^ A property is a type annotation that is used to
--   specify on which type a function can be called.
type Generic = Text

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
  = TFunction [Generic] (Annoted Type) [Argument] (Located Expression)
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
  | TFunctionProp
      [Generic]
      Property
      (Annoted Type)
      [Argument]
      (Located Expression)
  -- ^ TFunctionProp denotes a function declaration with a property
  --   The first argument is a list of generic type variables
  --   The second argument is the property
  --   The third argument is the return type
  --   The fourth argument is a list of arguments
  --   The fifth argument is the body of the function
  | TUse Path
  -- ^ TUse denotes a use declaration
  --   The argument is the path to the module
  | TProperty [Generic] Property (Annoted Type) [Argument]
  -- ^ TProperty denotes a property declaration
  --   The first argument is a list of generic type variables
  --   The second argument is the property
  --   The third argument is the return type
  --   The fourth argument is a list of arguments
  | TNamespace Name [Located Toplevel]
  -- ^ TNamespace denotes a namespace
  --   The first argument is the name of the namespace
  --   The second argument is a list of toplevel declarations
  | TAnnotation Text (Located Toplevel)
  | TEnumeration (Annoted [Generic]) [Annoted [Type]]
  | TTypeAlias (Annoted [Generic]) Type
  deriving (Eq, Show)

data Expression
  = EVariable Namespaced
  -- ^ EVariable denotes a variable
  --   The argument is the name of the variable
  | EApplication (Located Expression) [Located Expression]
  -- ^ EApplication denotes a function application
  --   The first argument is the function
  --   The second argument is a list of arguments
  | ELet (Annoted Type) (Located Expression) (Maybe (Located Expression))
  -- ^ ELet denotes a let binding
  --   The first argument is the name of the variable
  --   The second argument is the value of the variable
  --   The third argument is the body of the let binding
  | EIf (Located Expression) [Located Expression] [Located Expression]
  -- ^ EIf denotes an if expression
  --   The first argument is the condition
  --   The second argument is the then branch
  --   The third argument is the else branch
  | ELiteral Literal
  -- ^ ELiteral denotes a literal
  --   The argument is the literal
  | EProperty (Located Expression) Name
  -- ^ EProperty denotes a property access
  --   The first argument is the object
  --   The second argument is the name of the property
  | EFunction Type [Argument] (Located Expression)
  -- ^ EFunction denotes a function
  --   The first argument is a list of arguments
  --   The second argument is the body of the function
  | EStruct Type [Annoted (Located Expression)]
  -- ^ EStruct denotes a struct
  --   The first argument is a list of generic type variables
  --   The second argument is a list of fields
  | EList [Located Expression]
  -- ^ EList denotes a list
  --   The argument is a list of elements
  | EIndex (Located Expression) (Located Expression)
  -- ^ EIndex denotes an index access
  --   The first argument is the object
  | EBlock [Located Expression]
  -- ^ EBlock denotes a block
  --   The argument is a list of expressions
  | EUpdate (Located UpdateExpression) (Located Expression)
  -- ^ EUpdate denotes an update expression
  --   The first argument is the update expression
  --   The second argument is the object
  | EDereference (Located Expression)
  -- ^ EDereference denotes a dereference
  --   The argument is the object
  | EReference (Located Expression)
  -- ^ EReference denotes a reference
  --   The argument is the object
  | EFor Name (Located Expression) (Located Expression) [Located Expression]
  -- ^ EFor denotes a for loop
  --   The first argument is the name of the variable
  --   The second argument is the iterator start value
  --   The third argument is the iterator end value
  --   The fourth argument is the body of the for loop
  | EWhile (Located Expression) [Located Expression]
  -- ^ EWhile denotes a while loop
  --   The first argument is the condition
  --   The second argument is the body of the while loop
  | ESizeOf Type
  -- ^ ESizeOf denotes a sizeof expression
  --   The argument is the type
  | EAnnotation (Located Expression) Type
  -- ^ EAnnotation denotes a type annotation
  --   The first argument is the expression
  --   The second argument is the type
  | EAssembly Text [Located Expression]
  -- ^ EAssembly denotes an assembly expression
  --   The first argument is the assembly code
  --   The second argument is a list of arguments
  | EMatch (Located Expression) [Match]
  -- ^ EMatch denotes a match expression
  --   The first argument is the expression
  --   The second argument is a list of matches
  | EHole
  -- ^ EHole denotes a hole
  --   The argument is the name of the hole
  deriving (Eq, Show)

type Match = (Located Pattern, Located Expression)

data Pattern
  = PVariable Namespaced
  -- ^ PVariable denotes a variable
  --   The argument is the name of the variable
  | PWildcard
  -- ^ PWildcard denotes a wildcard
  | PLiteral Literal
  -- ^ PLiteral denotes a literal
  --   The argument is the literal
  | PStruct Type [Annoted (Located Pattern)]
  -- ^ PStruct denotes a struct
  --   The first argument is a list of generic type variables
  --   The second argument is a list of fields
  | PApp Namespaced [Located Pattern]
  -- ^ PApp denotes a constructor application
  --   The first argument is the name of the constructor
  --   The second argument is a list of arguments
  deriving (Eq, Show)

data UpdateExpression
  = UVariable Namespaced
  -- ^ UVariable denotes a variable
  --   The argument is the name of the variable
  | UProperty (Located UpdateExpression) Name
  -- ^ UProperty denotes a property access
  --   The first argument is the object
  --   The second argument is the name of the property
  | UIndex (Located UpdateExpression) (Located Expression)
  -- ^ UIndex denotes an index access
  --   The first argument is the object
  --   The second argument is the index
  | UDereference (Located UpdateExpression)
  deriving (Eq, Show)
