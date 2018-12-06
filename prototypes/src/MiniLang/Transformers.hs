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

-- Eval 2
type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runExceptT

eval2 :: Env -> Exp -> Eval2 Value

eval2 env (Lit num) = return $ IntVal num

-- eval2 env (Var name) = return . fromJust . M.lookup name $ env
eval2 env (Var name) = case M.lookup name env of
  Nothing -> throwError $ "unbounded variable: " ++ name
  Just v -> return v

eval2 env (Plus exp1 exp2) = do
  v1 <- eval2 env exp1
  v2 <- eval2 env exp2
  case (v1, v2) of
    (IntVal num1, IntVal num2) -> return . IntVal $ num1 + num2
    _ -> throwError "type error in addition"

eval2 env (Abs name exp) = return $ FunVal env name exp

eval2 env (App funcExp exp) = do
  v1 <- eval2 env funcExp
  v2 <- eval2 env exp
  case v1 of
    FunVal funcEnv name body -> eval2 (M.insert name v2 funcEnv) body
    _ -> throwError "type error in application"


exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

