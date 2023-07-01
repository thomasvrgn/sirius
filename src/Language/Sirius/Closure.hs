module Language.Sirius.Closure where

import qualified Control.Monad.RWS                        as RWS
import           Language.Sirius.Closure.Conversion       (convertToplevel)
import           Language.Sirius.Closure.Monad            (ClosureState (..))
import qualified Language.Sirius.Typecheck.Definition.AST as A

runClosureConversionPass :: Monad m => [A.Toplevel] -> m [A.Toplevel]
runClosureConversionPass xs =
  snd <$>
  RWS.execRWST
    (mapM convertToplevel xs)
    ()
    (ClosureState {clCounter = 0, clExcluded = mempty, clToplevels = xs, clConverted = mempty})
