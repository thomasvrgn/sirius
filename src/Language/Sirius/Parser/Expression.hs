module Language.Sirius.Parser.Expression where

import qualified Control.Applicative                      as A
import           Data.List                                (foldr1)
import qualified Language.Sirius.CST.Expression           as C
import qualified Language.Sirius.CST.Modules.Annoted      as C
import qualified Language.Sirius.CST.Modules.Located      as C
import qualified Language.Sirius.Parser.Lexer             as L
import qualified Language.Sirius.Parser.Modules.Literal   as L
import           Language.Sirius.Parser.Modules.Operators (operatorTable)
import qualified Language.Sirius.Parser.Modules.Type      as T
import qualified Text.Parsec                              as P
import qualified Text.Parsec.Expr                         as E
import Language.Sirius.CST.Modules.Type (Type(TypeApp))
import qualified Language.Sirius.CST.Modules.Namespaced as D
import Language.Sirius.Parser.Modules.Literal (parseNamespaced)
import Language.Sirius.Parser.Lexer (stringLiteral)
import Language.Sirius.Parser.Modules.Pattern (parsePattern)

parseLiteral :: Monad m => L.Sirius m C.Expression
parseLiteral = L.lexeme $ C.ELiteral <$> L.parseLiteral

parseIdentifier :: Monad m => L.Sirius m C.Expression
parseIdentifier = L.lexeme $ C.EVariable <$> parseNamespaced

parseList :: Monad m => L.Sirius m C.Expression
parseList = L.lexeme $ C.EList <$> L.brackets (P.sepBy parseExpression L.comma)

parseSizeof :: Monad m => L.Sirius m C.Expression
parseSizeof = L.lexeme $ C.ESizeOf <$> (L.reserved "sizeof" *> T.parseType)

parseFor :: Monad m => L.Sirius m C.Expression
parseFor =
  L.lexeme $ do
    L.reserved "for"
    name <- L.identifier
    L.reserved "in"
    start <- parseExpression
    L.reservedOp "to"
    end <- parseExpression
    exprs <- L.braces $ P.many (parseExpression <* P.optionMaybe L.semi)
    return $ C.EFor name start end exprs

parseWhile :: Monad m => L.Sirius m C.Expression
parseWhile =
  L.lexeme $ do
    L.reserved "while"
    cond <- parseExpression
    exprs <- L.braces $ P.many (parseExpression <* P.optionMaybe L.semi)
    return $ C.EWhile cond exprs

parseIf :: Monad m => L.Sirius m C.Expression
parseIf =
  L.lexeme $ do
    L.reserved "if"
    cond <- parseExpression
    exprs <- L.braces $ P.many (parseExpression <* P.optionMaybe L.semi)
    elseExprs <- P.option [] $ L.reserved "else" *> L.braces (P.many (parseExpression <* P.optionMaybe L.semi))
    return $ C.EIf cond exprs elseExprs

parseLambda :: Monad m => L.Sirius m C.Expression
parseLambda =
  L.lexeme $ do
    L.reserved "fn"
    args <-
      L.parens $
      L.commaSep
        (C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType)
    ret <- L.reservedOp ":" *> T.parseType
    C.EFunction ret args <$> parseExpression

parseLet :: Monad m => L.Sirius m C.Expression
parseLet =
  L.lexeme $ do
    L.reserved "let"
    name <- C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType
    L.reservedOp "="
    value <- parseExpression
    body <- P.optionMaybe $ L.reserved "in" *> parseExpression
    return $ C.ELet name value body

parseLetMut :: Monad m => L.Sirius m C.Expression
parseLetMut =
  L.lexeme $ do
    L.reserved "let"
    L.reserved "ref"
    name <- C.Annoted <$> L.identifier <* L.reservedOp ":" <*> (TypeApp (D.Simple "Address") . (:[]) <$> T.parseType)
    L.reservedOp "="
    value <- parseExpression
    body <- P.optionMaybe $ L.reserved "in" *> parseExpression
    return $ C.ELet name value body

parseBlock :: Monad m => L.Sirius m C.Expression
parseBlock =
  L.lexeme $ do
    body <- L.braces $ P.many (parseExpression <* P.optionMaybe L.semi)
    return $ C.EBlock body

makeUnaryOp :: A.Alternative f => f (a -> a) -> f (a -> a)
makeUnaryOp s = foldr1 (.) . reverse <$> A.some s

parseExpression :: Monad m => L.Sirius m C.Expression
parseExpression = do
  table <- operatorTable
  E.buildExpressionParser (newTable ++ table ++ [[E.Prefix update]]) parseTerm
  where
    newTable = [[E.Postfix $ makeUnaryOp postfix], [E.Prefix $ makeUnaryOp (deref P.<|> ref)]]
    deref = do
      s <- P.getPosition
      _ <- P.char '*'
      return $ \x@(C.Located (_, e) _) -> C.EDereference x C.:>: (s, e)
    ref = do
      s <- P.getPosition
      _ <- L.reserved "ref"
      return $ \x@(C.Located (_, e) _) -> C.EReference x C.:>: (s, e)
    postfix = cast P.<|> functionCall P.<|> dotProperty P.<|> arrayProperty P.<|> ptrProperty P.<|> match

    match = do
      L.reserved "match"
      cases <- L.braces $ P.many (parseCase <* P.optionMaybe L.comma)
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.EMatch x cases C.:>: (s, e)
      where
        parseCase = do
          pat <- parsePattern
          L.reservedOp "=>"
          expr <- parseExpression
          return (pat, expr)
    cast = do
      L.reserved "as"
      t <- T.parseType
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.EAnnotation x t C.:>: (s, e)
    dotProperty = do
      _ <- P.char '.'
      name <- L.identifier <|> L.operator
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.EProperty x name C.:>: (s, e)
    ptrProperty = do
      _ <- L.reservedOp "->"
      name <- L.identifier
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.EProperty (C.EDereference x C.:>: (s, e)) name C.:>: (s, e)
    arrayProperty = do
      index <- L.brackets parseExpression
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.EApplication (C.EProperty x "index" C.:>: (s, e)) [index] C.:>: (s, e)
    functionCall = do
      args <- L.parens $ L.commaSep parseExpression
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.EApplication x args C.:>: (s, e)

    update = do
      p <- P.getPosition
      updateExpr <- P.try $ parseUpdateVariable <* L.reservedOp "="
      return $ \x@(C.Located (_, e) _) -> C.EUpdate updateExpr x C.:>: (p, e)

parseAssembly :: Monad m => L.Sirius m C.Expression
parseAssembly = L.lexeme $ do
  L.reserved "asm"
  op <- fromString <$> stringLiteral
  args <- some parseExpression
  return $ C.EAssembly op args

parseTerm :: Monad m => L.Sirius m C.Expression
parseTerm =
  P.choice
    [ parseUpdate
    , L.parens parseExpression
    , parseStruct
    , parseAssembly
    , parseFunction
    , parseIf
    , parseSizeof
    , parseFor
    , parseWhile
    , parseBlock
    , parseLiteral
    , parseIdentifier
    , parseList
    , parseLambda
    , P.try parseLetMut
    , parseLet
    ]

parseStruct :: Monad m => L.Sirius m C.Expression
parseStruct = L.lexeme $ do
  (name, fields) <- P.try $ (,) <$> T.parseType <*> L.braces (L.commaSep (C.Annoted <$> L.identifier <* L.reservedOp "=" <*> parseExpression))
  return $ C.EStruct name fields

parseFunction :: Monad m => L.Sirius m C.Expression
parseFunction = L.lexeme $ do
  L.reserved "fn"
  args <-
    L.parens $
    L.commaSep
      (C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType)
  ret <- L.reservedOp ":" *> T.parseType
  L.reservedOp "="
  C.EFunction ret args <$> parseExpression

parseUpdateVariable :: Monad m => L.Sirius m C.UpdateExpression
parseUpdateVariable = E.buildExpressionParser table parseUTerm
  where
    table = [[E.Postfix $ makeUnaryOp postfix], [E.Prefix $ makeUnaryOp deref]]
    deref = do
      s <- P.getPosition
      _ <- P.char '*'
      return $ \x@(C.Located (_, e) _) -> C.UDereference x C.:>: (s, e)
    postfix = dotProperty P.<|> arrayProperty P.<|> ptrProperty
    dotProperty = do
      _ <- P.char '.'
      name <- L.identifier
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.UProperty x name C.:>: (s, e)
    ptrProperty = do
      _ <- L.reservedOp "->"
      name <- L.identifier
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.UProperty (C.UDereference x C.:>: (s, e)) name C.:>: (s, e)
    arrayProperty = do
      index <- L.brackets parseExpression
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.UIndex x index C.:>: (s, e)

parseUTerm :: Monad m => L.Sirius m C.UpdateExpression
parseUTerm =
  P.choice
    [ L.parens parseUpdateVariable
    , parseUVariable
    ]

parseUVariable :: Monad m => L.Sirius m C.UpdateExpression
parseUVariable = L.lexeme $ C.UVariable <$> parseNamespaced

parseUpdate :: Monad m => L.Sirius m C.Expression
parseUpdate = L.lexeme $ do
  name <- P.try $ parseUpdateVariable <* L.reservedOp "="
  C.EUpdate name <$> parseExpression