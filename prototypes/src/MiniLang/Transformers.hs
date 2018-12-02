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

-- Eval 1
type Eval1 a = Identity a

runEval :: Eval1 a -> a
runEval = runIdentity

eval1 :: Env -> Exp -> Eval1 Value

eval1 env (Lit num) = return $ IntVal num

eval1 env (Var name) = return . fromJust . M.lookup name $ env

eval1 env (Plus exp1 exp2) = do
  IntVal num1 <- eval1 env exp1
  IntVal num2 <- eval1 env exp2
  return . IntVal $ num1 + num2
eval1 env (Abs name exp) = return $ FunVal env name exp

eval1 env (App funcExp exp) = do
  FunVal funcEnv name body <- eval1 env funcExp
  val <- eval1 env exp
  eval1 (M.insert name val funcEnv) body

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

