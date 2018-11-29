module Logic.Satisfiable where

import qualified Data.Map as M
import Data.List

-- Ex3. determine whether a compound proposition is satisfiable


-- type alias and data definition
type Symbol = String

data CompoundProp = Prop Symbol 
  | And CompoundProp CompoundProp 
  | Or CompoundProp CompoundProp 
  | Not CompoundProp deriving (Show, Read)

type Func = ([Bool] -> Bool, [Symbol])

-- core of this module, it parse a proposition structure to a `Func`
parseToFunc :: CompoundProp -> Func
parseToFunc (Prop sym) = (head, [sym])
parseToFunc (And cp1 cp2) = 
  let func1@(f1, sym1) = parseToFunc cp1
      func2@(f2, sym2) = parseToFunc cp2
      sym = union sym1 sym2
  in (andFGenerator func1 func2 sym, sym)


-- apply `Func` to some arguments  
applyFunc :: Func -> M.Map Symbol Bool -> Bool
applyFunc (f, xs) m = f $ map ((M.!) m)  xs


getArguments :: [Symbol] -> [Symbol] -> [Bool] -> [Bool]
getArguments sym1 sym bools = 
  let argMap = M.fromList (zip sym bools)
      action cur acc = if M.member cur argMap then (argMap M.! cur) : acc else acc
  in foldr action [] sym1

-- all kinds of logic function generators, which transform `Func` to real function
andFGenerator :: Func -> Func -> [Symbol] -> ([Bool] -> Bool)
andFGenerator func1@(f1, sym1) func2@(f2, sym2) sym bools = 
  let params1 = getArguments sym1 sym bools
      params2 = getArguments sym2 sym bools
  in f1 params1 && f2 params2



p = Prop "p"
q = Prop "q"

pAndQ = And p q

pAndQAndPAndQ = And pAndQ pAndQ