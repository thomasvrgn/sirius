{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Sirius.Module.Monad where
import Language.Sirius.CST.Expression
import Language.Sirius.CST.Modules.Located
import Control.Monad.Except

type MonadBundling m = (MonadIO m, MonadError (Text, Position) m, MonadState BundlingState m)

data BundlingState = BundlingState {
  modules      :: [(Text, [Located Toplevel])],
  mappings     :: Map Text Text,
  types        :: Map Text Text,
  counter      :: Integer,
  currentPaths :: [Text]
} deriving Show

emptyBundling :: BundlingState
emptyBundling = BundlingState [] mempty mempty 0 []

fresh :: MonadBundling m => m Integer
fresh = do
  n <- gets counter
  modify $ \s -> s { counter = n + 1 }
  return n

freshName :: MonadBundling m => Text -> m Text
freshName name = do
  n <- fresh
  return $ "$m" <> show n <> "_" <> name

freshType :: MonadBundling m => Text -> m Text
freshType name = do
  n <- fresh
  return $ "$t" <> show n <> "_" <> name
