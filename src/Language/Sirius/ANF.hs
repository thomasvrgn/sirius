module Language.Sirius.ANF where

import qualified Language.Sirius.ANF.AST                  as ANF
import           Language.Sirius.ANF.Conversion           (convertToplevel)
import qualified Language.Sirius.Typecheck.Definition.AST as C
import qualified Control.Monad.RWS as ST

runANFPass :: Monad m => [C.Toplevel] -> m [ANF.Toplevel]
runANFPass xs = fst <$> ST.evalRWST (mapM convertToplevel xs) [] 0
