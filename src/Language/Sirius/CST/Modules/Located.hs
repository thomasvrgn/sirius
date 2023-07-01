{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sirius.CST.Modules.Located where

import           Text.Parsec.Pos (SourcePos)
import qualified Text.Show       as T

type Position = (SourcePos, SourcePos)

data Located a =
  Located
    { loc   :: Position
    , unLoc :: a
    }

instance Eq a => Eq (Located a) where
  Located _ x == Located _ y = x == y

pattern (:>:) :: a -> Position -> Located a

pattern a :>: pos = Located pos a

instance T.Show a => T.Show (Located a) where
  show (Located _ x) = show x
