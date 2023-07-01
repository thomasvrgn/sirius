module Language.Sirius.Parser.Modules.Type where

import qualified Data.Functor                     as F
import qualified Language.Sirius.CST.Modules.Type as D
import qualified Language.Sirius.Parser.Lexer     as L
import qualified Text.Parsec                      as P
import Language.Sirius.Parser.Modules.Literal (parseNamespaced)
import qualified Language.Sirius.CST.Modules.Namespaced as D

parsePrimitive :: Monad m => L.Parser m D.Type
parsePrimitive =
  P.choice
    [ parseTypeInt
    , parseTypeFloat
    , L.reserved "Bool" F.$> D.TypeBool
    , L.reserved "Char" F.$> D.TypeChar
    , L.reserved "Void" F.$> D.TypeVoid
    , L.reserved "String" F.$> D.TypeApp (D.Simple "List") [D.TypeChar]
    ]

parseMutable :: Monad m => L.Parser m D.Type
parseMutable = do
  L.reserved "ref"
  D.TypeApp (D.Simple "Address") . (: []) <$> parseType

parseFunction :: Monad m => L.Parser m D.Type
parseFunction = do
  L.reserved "fn"
  args <- L.parens $ L.commaSep parseType
  L.reservedOp ":"
  D.TypeFunction args <$> parseType

parseTypeInt :: Monad m => L.Parser m D.Type
parseTypeInt = L.reserved "Int" F.$> D.TypeInt

parseTypeFloat :: Monad m => L.Parser m D.Type
parseTypeFloat = L.reserved "Float" F.$> D.TypeFloat

parseTypeList :: Monad m => L.Parser m D.Type
parseTypeList = do
  ty <- L.brackets parseType
  return $ D.TypeApp (D.Simple "List") [ty]

parseTypeApp :: Monad m => L.Parser m D.Type
parseTypeApp = do
  name <- parseNamespaced
  args <- L.brackets $ L.commaSep parseType
  return $ D.TypeApp name args

parseTypeVar :: Monad m => L.Parser m D.Type
parseTypeVar = D.TypeVar <$> parseNamespaced

parseType :: Monad m => L.Parser m D.Type
parseType =
  P.choice
    [ parsePrimitive
    , parseTypeList
    , parseFunction
    , parseMutable
    , P.try parseTypeApp
    , parseTypeVar
    ]
