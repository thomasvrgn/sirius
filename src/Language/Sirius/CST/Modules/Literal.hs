module Language.Sirius.CST.Modules.Literal where

import qualified Text.Show as T

data Literal
  = Char Char
  | String String
  | Int Integer
  | Float Double
  | Bool Bool
  deriving (Eq, Ord)

instance T.Show Literal where
  show (Char c)   = show c
  show (String s) = show s
  show (Int i)    = show i
  show (Float f)  = show f
  show (Bool b)   = show b
