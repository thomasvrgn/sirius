{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Language.Sirius.Module.Resolver where
import Language.Sirius.CST.Modules.Located
import Language.Sirius.CST.Expression
import Control.Monad.Except
import Language.Sirius.Parser
import System.Directory
import System.FilePath

type MonadResolver m = (MonadIO m, MonadError (Text, Position) m)

getSiriusPath :: MonadResolver m => Position -> m String
getSiriusPath pos = do
  path <- liftIO $ lookupEnv "SIRIUS"
  case path of
    Just path' -> return path'
    Nothing -> throwError ("Could not find SIRIUS environment variable", pos)

resolveImport :: MonadResolver m => Text -> Text -> Position -> m [Located Toplevel]
resolveImport dir name pos = do
  stdPath <- getSiriusPath pos
  let stdName = stdPath </> "standard" </> drop 4 (toString name)
  let path = (if "std" `isPrefixOf` toString name then stdName else toString dir </> toString name) -<.> "sirius"

  fileExists <- liftIO $ doesFileExist path
  unless fileExists $ throwError ("Could not find module " <> name, pos)

  content <- decodeUtf8 <$> liftIO (readFileBS path)
  ast <- parseSirius (toString name) content
  case ast of
    Left err -> liftIO $ print err >> exitFailure
    Right ast' -> resolveImports (fromString $ takeDirectory path) ast'

-- | Resolve the imports of a module.
resolveImports :: MonadResolver m => Text -> [Located Toplevel] -> m [Located Toplevel]
resolveImports dir (Located pos (TUse name) : toplevels) = do
  resolved <- resolveImport dir (fromString $ intercalate "/" (map toString name)) pos
  rest <- resolveImports dir toplevels
  return $ (TNamespace "_" resolved :>: pos) : rest
resolveImports dir (Located pos (TNamespace name toplevels) : toplevels') = do
  resolved <- resolveImports dir toplevels
  rest <- resolveImports dir toplevels'
  return $ (TNamespace name resolved :>: pos) : rest
resolveImports dir (Located pos (TAnnotation name tl) : toplevels) = do
  rest <- resolveImports dir toplevels
  return $ (TAnnotation name tl :>: pos) : rest
resolveImports dir (toplevel : toplevels) = do
  rest <- resolveImports dir toplevels
  return $ toplevel : rest
resolveImports _ [] = return []

runModuleResolver :: MonadIO m => Text -> [Located Toplevel] -> m (Either (Text, Position) [Located Toplevel])
runModuleResolver dir toplevels = runExceptT (resolveImports (fromString $ takeDirectory (toString dir)) toplevels)