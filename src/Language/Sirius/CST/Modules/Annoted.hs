{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}

module Language.Sirius.CST.Modules.Annoted where

import qualified Text.Show as T

-- Representing a binding that binds a type representation
-- to a name.
data Annoted a =
  Annoted
    { annotedName :: Text
    , annotedType :: a
    }
  deriving (Eq, Ord, Functor)

pattern (:@) :: Text -> a -> Annoted a

pattern x :@ t = Annoted x t

instance T.Show a => T.Show (Annoted a) where
  show (x :@ t) = show x ++ " : " ++ show t
  show _        = "COMPILER ERROR: Annoted.show"

instance {-# OVERLAPS #-} T.Show (Annoted String) where
  show (x :@ t) = show x ++ " : " ++ t
  show _        = "COMPILER ERROR: Annoted.show"

instance {-# OVERLAPS #-} T.Show a => T.Show (Annoted (Maybe a)) where
  show (x :@ (Just t)) = show x ++ " : " ++ show t
  show (x :@ Nothing)  = show x
  show _               = "COMPILER ERROR: Annoted.show"
