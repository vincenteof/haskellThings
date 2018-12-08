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

--eval 6
type Eval6 a = ReaderT Env (ExceptT String
                            (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = 
  runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit num) = do
  tick
  liftIO $ print num
  return $ IntVal num
eval6 (Var name) = do
  tick
  tell [name]
  env <- ask
  case M.lookup name env of
    Nothing -> throwError $ "unbounded variable: " ++ name
    Just v -> return v
eval6 (Plus e1 e2) = do
  tick
  v1 <- eval6 e1
  v2 <- eval6 e2
  case (v1, v2) of
    (IntVal num1, IntVal num2) -> return $ IntVal (num1 + num2)
    _ -> throwError "type error in addition"
eval6 (Abs name body) = do
  tick
  env <- ask
  return $ FunVal env name body
eval6 (App funcExp exp) = do
  tick
  fv <- eval6 funcExp
  paramV <- eval6 exp
  case fv of
    FunVal prevEnv name body -> local (const (M.insert name paramV prevEnv))
                                      (eval6 body)
    _ -> throwError "type error in application"

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)




