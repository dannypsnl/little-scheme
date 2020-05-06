module Scheme.NewInterpreter () where
import Data.IORef
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Scheme.Ast.Core
import Scheme.Core hiding (Env, ScmValue(..))

-- eval
evalCore :: Env -> Core -> IOThrowsError Core
evalCore env v@(CoreNumber _ _) = return v
evalCore env v@(CoreBool _ _) = return v
evalCore env v@(CoreString _ _) = return v

-- Env
type Env = IORef [Map String (IORef Core)]
