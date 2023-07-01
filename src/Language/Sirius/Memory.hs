module Language.Sirius.Memory where
import qualified Language.Sirius.CST.Expression as C
import Language.Sirius.Memory.Management (analyseToplevel, malloc)
import qualified Language.Sirius.CST.Modules.Located as C
import qualified Control.Monad.RWS as ST

runMemoryPass :: Monad m => [C.Located C.Toplevel] -> m [C.Located C.Toplevel]
runMemoryPass xs = fst <$> ST.evalRWST (mapM (\(x C.:>: pos) -> C.Located pos <$> analyseToplevel x) xs) False ([], malloc, mempty)