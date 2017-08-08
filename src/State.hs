module State (
  State.State,
  State.runState
) where

import Control.Monad.State
import Control.Monad.Trans.Except

import Language.Cython.Error

type State ctx annot = ExceptT (Error annot) (Control.Monad.State.State ctx)

runState :: State.State ctx annot a -> ctx -> State.State ctx annot (a, ctx)
runState s c = do
  let (newState, newCtx) = Control.Monad.State.runState (runExceptT s) c
  either throwE (\r -> return (r, newCtx)) newState
