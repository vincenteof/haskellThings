module Logic.Satisfiable where

-- Ex3. determine whether a compound proposition is satisfiable

newtype Symbol = Symbol { runSymbol :: Char }

data CompoundProp = Prop Symbol 
  | And CompoundProp CompoundProp 
  | Or CompoundProp CompoundProp 
  | Not CompoundProp


-- So the problem becomes how to respresent a function whose num of arguments is arbitary
parse :: CompoundProp -> (Bool -> Bool)
parse = undefined