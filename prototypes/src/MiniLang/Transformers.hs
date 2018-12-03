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

eval2a :: Env -> Exp -> Eval2 Value

eval2a env (Lit num) = return $ IntVal num

eval2a env (Var name) = return . fromJust . M.lookup name $ env

eval2a env (Plus exp1 exp2) = do
  IntVal num1 <- eval2a env exp1
  IntVal num2 <- eval2a env exp2
  return . IntVal $ num1 + num2

eval2a env (Abs name exp) = return $ FunVal env name exp

eval2a env (App funcExp exp) = do
  FunVal funcEnv name body <- eval2a env funcExp
  val <- eval2a env exp
  eval2a (M.insert name val funcEnv) body

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)
