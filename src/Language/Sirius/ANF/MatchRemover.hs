{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Language.Sirius.ANF.MatchRemover where
import qualified Language.Sirius.Typecheck.Definition.AST as C
import qualified Language.Sirius.ANF.AST as ANF
import qualified Language.Sirius.CST.Modules.Literal as C
import qualified Language.Sirius.CST.Modules.Annoted as C
import Data.Text (toLower)

pattern EBinary :: Text -> ANF.Expression -> ANF.Expression -> ANF.Expression
pattern EBinary op a b = ANF.EApplication op [a, b]

findEnumerationByVariant :: [ANF.Toplevel] -> Text -> Maybe Int
findEnumerationByVariant xs name = do
  findMatch name xs
  where
    findMatch :: Text -> [ANF.Toplevel] -> Maybe Int
    findMatch _ [] = Nothing
    findMatch name' (ANF.TUnion _ variants : xs') = do
      if toLower name' `elem` map (toLower . C.annotedName) variants
        then Just $ getIndex (map (toLower . C.annotedName) variants) (toLower name')
        else findMatch name' xs'
    findMatch name' (_ : xs') = findMatch name' xs'

getIndex :: Eq a => [a] -> a -> Int
getIndex xs x = go xs x 0
  where
    go :: Eq a => [a] -> a -> Int -> Int
    go [] _ _ = -1
    go (x'':xs') x' i = if x' == x'' then i else go xs' x' (i + 1)

findPattern :: [ANF.Toplevel] -> C.Pattern -> (ANF.Expression -> [(Maybe ANF.Expression, Maybe ANF.Expression)])
findPattern _ (C.PLiteral l) = \expr -> [(Nothing, Just (case l of
   C.Char c -> EBinary "==" expr $ ANF.ELiteral (C.Int $ fromIntegral $ fromEnum c)
   C.String _ -> EBinary "==" (ANF.EApplication "strcmp" [expr, ANF.ELiteral l]) (ANF.ELiteral (C.Int 0))
   C.Int _ -> EBinary "==" expr $ ANF.ELiteral l
   C.Float _ -> ANF.EAssembly "fcmp" [expr, ANF.ELiteral l]
   C.Bool _ -> EBinary "==" expr $ ANF.ELiteral l)
  )]
findPattern _ C.PWildcard = const [(Nothing, Nothing)]
findPattern tls (C.PVariable v' t) = \expr -> do
  case findEnumerationByVariant tls v' of
    Just _ -> [(Nothing, Just $ EBinary "==" (ANF.EApplication "strcmp" [ANF.EInternalField (ANF.EInternalField expr 0) 0, ANF.ELiteral (C.String $ toString v')]) (ANF.ELiteral (C.Int 0)))]
    _ -> [(Just $ ANF.ELet (C.Annoted v' t) expr, Nothing)]
findPattern e (C.PApp name xs _) = do
  \expr -> do
    -- Used in the case of enumerations:
    -- Searching through the list of variants to find the index of the variant
    -- It is used to then codegen correctly the pattern matching
    -- let i = fromJust $ findEnumerationByVariant e name
    let xs' = zipWith (\x y -> findPattern e y (ANF.EInternalField (ANF.EInternalField expr 0) x)) [1..] xs

    concat ([(Nothing, Just $ EBinary "==" (ANF.EApplication "strcmp" [ANF.EInternalField (ANF.EInternalField expr 0) 0, ANF.ELiteral (C.String $ toString name)]) (ANF.ELiteral (C.Int 0)))] : xs')
findPattern env (C.PStruct _ fields) = \expr -> do
  concatMap (\(C.Annoted name e) -> findPattern env e (ANF.EProperty expr name)) fields
