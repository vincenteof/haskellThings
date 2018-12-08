module MiniLang.Transformers where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as M

-- type alias and data definition
type Name = String

data Exp = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving Show

data Value = IntVal Integer
  | FunVal Env Name Exp
  deriving Show

type Env = M.Map Name Value

-- eval 4
type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit num) = do
  tick
  return $ IntVal num
eval4 (Var name) = do
  tick
  env <- ask
  case M.lookup name env of
    Nothing -> throwError $ "unbounded variable: " ++ name
    Just v -> return v
eval4 (Plus exp1 exp2) = do
  tick
  v1 <- eval4 exp1
  v2 <- eval4 exp2
  case (v1, v2) of
    (IntVal num1, IntVal num2) -> return $ IntVal (num1 + num2)
    _ -> throwError "type error in addition"
eval4 (Abs paramName body) = do
  tick
  env <- ask
  return $ FunVal env paramName body
eval4 (App funcExp exp) = do
  tick
  fv <- eval4 funcExp
  paramV <- eval4 exp
  case fv of
    FunVal prevEnv name body -> local (const (M.insert name paramV prevEnv))
                                      (eval4 body)
    _ -> throwError "type error in application"


exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)



