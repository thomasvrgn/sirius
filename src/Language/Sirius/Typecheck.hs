module Language.Sirius.Typecheck where

import qualified Control.Monad.Except                       as E
import qualified Control.Monad.State                        as ST
import qualified Data.Map                                   as M
import qualified Language.Sirius.CST.Expression             as C
import qualified Language.Sirius.CST.Modules.Located        as C
import qualified Language.Sirius.Typecheck.Checker          as I
import qualified Language.Sirius.Typecheck.Definition.AST   as A
import           Language.Sirius.Typecheck.Definition.Monad (CheckerState (..))
import qualified Language.Sirius.Typecheck.Definition.Type  as T
import qualified Data.Set as S

runInferencePass ::
     Monad m
  => [C.Located C.Toplevel]
  -> m (Either (Text, Maybe Text, C.Position) ([A.Toplevel], CheckerState))
runInferencePass toplevels =
  E.runExceptT $
  ST.runStateT
    (do
      xs <- catMaybes <$> mapM ((snd <$>) . I.inferToplevel) toplevels
      s <- ST.gets holes
      if S.null s
        then return xs
        else mapM (\(pos, t) -> E.throwError ("Unresolved holes, receiving " <> show t, Nothing, pos)) (S.toList s)
    )
    (mempty {variables = functions})

functions :: M.Map Text T.Scheme
functions = M.fromList [("void", T.Forall [] T.Void)]
