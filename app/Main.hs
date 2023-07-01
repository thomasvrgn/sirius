{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Language.Sirius.Parser
import Language.Sirius.Typecheck
import Language.Sirius.Monomorphize (runMonomorphizationPass)
import Language.Sirius.Closure (runClosureConversionPass)
import Language.Sirius.LLVM (runCompilerPass, getLLContent)
import Language.Sirius.Module.Bundler (runModuleBundling)
import Language.Sirius.ANF (runANFPass)
import Language.Sirius.Memory (runMemoryPass)

main :: IO ()
main = do
  let file = "example/index.sirius"
  contents <- readFileBS file
  res <- parseSirius file (decodeUtf8 contents)
  case res of
    Left err -> print err
    Right toplevels -> do
      res' <- runModuleBundling toplevels
      case res' of
        Left err -> print err
        Right toplevels -> do
          res'' <- runMemoryPass toplevels
          -- mapM_ print res''
          res' <- runInferencePass res''
          case res' of
            Left err -> print err <* putStrLn "Typecheck failed"
            Right (ast, checker) -> do 
              -- mapM_ print ast
              res'' <- runMonomorphizationPass ast checker
              case res'' of
                Left err -> print err
                Right res'' -> do
                  res'' <- runClosureConversionPass res''
                  res'' <- runANFPass res''
                  mapM_ print res''
                  content <- getLLContent res''
                  writeFileBS "out.ll" content
                  runCompilerPass res''