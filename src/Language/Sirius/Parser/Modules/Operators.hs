{-# LANGUAGE OverloadedStrings #-}

module Language.Sirius.Parser.Modules.Operators where

import qualified Language.Sirius.CST.Expression      as C
import qualified Language.Sirius.CST.Modules.Located as C
import qualified Language.Sirius.Parser.Lexer        as L
import qualified Text.Parsec                         as P
import qualified Text.Parsec.Expr                    as E

operatorTable ::
     Monad m => L.Parser m [[E.Operator Text () m (C.Located C.Expression)]]
operatorTable = do
  return $ reverse
    [ [propCallBin "||" E.AssocLeft, propCallBin "&&" E.AssocLeft]
    , [propCallBin "!=" E.AssocLeft, propCallBin "==" E.AssocLeft]
    , [ propCallBin ">=" E.AssocLeft
      , propCallBin ">" E.AssocLeft
      , propCallBin "<=" E.AssocLeft
      , propCallBin "<" E.AssocLeft
      ]
    , [propCallBin "+" E.AssocLeft, propCallBin "-" E.AssocLeft]
    , [ propCallBin "%" E.AssocLeft
      , propCallBin "/" E.AssocLeft
      , propCallBin "*" E.AssocLeft
      ]
    , [propCallUn "-", propCallUn "+"]
    , [propCallUn "!", propCallUn "--", propCallUn "++"]
    , [propCallPost "--", propCallPost "++"]
    , [common]
    ]

propCallBin ::
     Monad m
  => String
  -> E.Assoc
  -> E.Operator Text () m (C.Located C.Expression)
propCallBin name =
  binary
    name
    (\x@(C.Located pos _) y ->
       C.EApplication (C.EProperty x (fromString name) C.:>: pos) [y])

propCallUn :: Monad m => String -> E.Operator Text () m (C.Located C.Expression)
propCallUn name =
  prefix
    name
    (\x@(C.Located pos _) ->
       C.EApplication (C.EProperty x (fromString name) C.:>: pos) [])

propCallPost ::
     Monad m => String -> E.Operator Text () m (C.Located C.Expression)
propCallPost name =
  postfix
    name
    (\x@(C.Located pos _) ->
       C.EApplication (C.EProperty x (fromString name) C.:>: pos) [])

common :: Monad m => E.Operator Text () m (C.Located C.Expression)
common =
  E.Infix
    (do op <- L.operator
        return $ \x@(C.Located (s, _) _) y@(C.Located (_, e) _) ->
          C.EApplication (C.EProperty x op C.:>: (s, s)) [y] C.:>: (s, e)) E.AssocRight

binary ::
     Monad m
  => String
  -> (C.Located a -> C.Located a -> a)
  -> E.Assoc
  -> E.Operator Text () m (C.Located a)
binary name f =
  E.Infix
    (do L.reservedOp name
        return $ \x@(C.Located (s, _) _) y@(C.Located (_, e) _) ->
          f x y C.:>: (s, e))

prefix ::
     Monad m
  => String
  -> (C.Located a -> a)
  -> E.Operator Text () m (C.Located a)
prefix name f =
  E.Prefix
    (do s <- P.getPosition
        L.reservedOp name
        return $ \x@(C.Located (_, e) _) -> f x C.:>: (s, e))

postfix ::
     Monad m
  => String
  -> (C.Located a -> a)
  -> E.Operator Text () m (C.Located a)
postfix name f =
  E.Postfix
    (do L.reservedOp name
        e <- P.getPosition
        return $ \x@(C.Located (s, _) _) -> f x C.:>: (s, e))
