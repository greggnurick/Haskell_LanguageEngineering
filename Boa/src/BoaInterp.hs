-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'
module BoaInterp
  ( Env
  , RunError(..)
  , Comp(..)
  , abort
  , look
  , withBinding
  , output
  , truthy
  , operate
  , apply
  , eval
  , exec
  , execute
  ) where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError
  = EBadVar VName
  | EBadFun FName
  | EBadArg String
  deriving (Eq, Show)

newtype Comp a =
  Comp
    { runComp :: Env -> (Either RunError a, [String])
    }

instance Monad Comp where
  return a = Comp $ const (Right a, [])
  m >>= f =
    Comp $ \env ->
      case runComp m env of
        (Left e, s) -> (Left e, s)
        (Right a, s) ->
          case runComp (f a) env of
            (Left e, s') -> (Left e, s ++ s')
            (Right a, s') -> (Right a, s ++ s')

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM

instance Applicative Comp where
  pure = return
  (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort e = Comp $ const (Left e, mempty)

look :: VName -> Comp Value
look var =
  Comp $ \env ->
    case lookup var env of
      Nothing -> (Left (EBadVar var), [])
      Just x -> (Right x, [])

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding var x comp = Comp $ \env -> runComp comp ((var, x) : env)

output :: String -> Comp ()
output s = Comp $ const (Right (), [s])

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy TrueVal = True
truthy FalseVal = False
truthy (IntVal x) = x /= 0
truthy (StringVal s) = s /= mempty
truthy (ListVal l) = l /= []

eitherToComp :: Either String Value -> Comp Value
eitherToComp (Left s) = Comp $ const (Right NoneVal, [s])
eitherToComp (Right x) = return x

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal x) (IntVal y) = Right (IntVal (x + y))
operate Minus (IntVal x) (IntVal y) = Right (IntVal (x - y))
operate Times (IntVal x) (IntVal y) = Right (IntVal (x * y))
operate Div (IntVal _) (IntVal 0) = Left "try again"
operate Div (IntVal x) (IntVal y) = Right (IntVal (div x y))
operate Mod (IntVal _) (IntVal 0) = Left "try again"
operate Mod (IntVal x) (IntVal y) = Right (IntVal (mod x y))
operate Less (IntVal x) (IntVal y) =
  if x < y
    then Right TrueVal
    else Right FalseVal
operate Greater (IntVal x) (IntVal y) =
  if x > y
    then Right TrueVal
    else Right FalseVal
operate In x (ListVal y) =
  if x `elem` y
    then Right TrueVal
    else Right FalseVal
operate Eq x y =
  if x == y
    then Right TrueVal
    else Right FalseVal
operate _ _ _ = Left "try again"

apply :: FName -> [Value] -> Comp Value
apply "range" x = rangeB x
apply "print" x = printB x
apply fname _ = abort (EBadFun fname)

showB :: Value -> String
showB NoneVal = "None"
showB TrueVal = "True"
showB FalseVal = "False"
showB (IntVal x) = show x
showB (StringVal s) = s
showB (ListVal []) = "[]"
showB (ListVal xs) = "[" ++ listB xs ++ "]"

listB :: [Value] -> String
listB [] = ""
listB [x] = showB x
listB (x:xs) = showB x ++ ", " ++ listB xs

printHelp :: [Value] -> String
printHelp [] = ""
printHelp [x] = showB x
printHelp (x:xs) = showB x ++ " " ++ printHelp xs

printB :: [Value] -> Comp Value
printB [] = do
  output ""
  return NoneVal
printB vs = do
  output (printHelp vs)
  return NoneVal

rangeB :: [Value] -> Comp Value
rangeB [IntVal x] = rangeB [IntVal 0, IntVal x, IntVal 1]
rangeB [IntVal x, IntVal y] = rangeB [IntVal x, IntVal y, IntVal 1]
rangeB [IntVal x, IntVal y, IntVal z]
  | z == 0 = abort (EBadArg "step size can't be zero")
  | y >= x = return (ListVal [IntVal xs | xs <- [x,x + z .. y], xs < y])
  | x >= y && z > 0 || x <= y && z < 0 = return (ListVal [])
  | x >= y = return (ListVal [IntVal xs | xs <- [x,x - (-z) .. y], xs > y])
rangeB _ = abort (EBadArg "not valid values")

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const x) = return x
eval (Var v) = look v
eval (Oper op x y) = do
  n <- eval x
  m <- eval y
  eitherToComp (operate op n m)
eval (Not exp) = do
  x <- eval exp
  if truthy x
    then return FalseVal
    else return TrueVal
eval (Call f exps) = do
  vs <- eval (List exps)
  case vs of
    (ListVal vs') -> apply f vs'
    _ -> abort (EBadArg "bad variable name")
eval (List []) = return (ListVal [])
eval (List (exp:exps)) = do
  v0 <- eval exp
  v1 <- eval (List exps)
  case v1 of
    (ListVal vs) -> return (ListVal (v0 : vs))
    _ -> abort (EBadArg "bad variable name")
eval (Compr _ []) = return (ListVal [])
eval (Compr exp0 ((QFor var exp):quals)) = do
  vs <- eval exp
  case vs of
    (ListVal []) -> eval (Compr exp0 quals)
    (ListVal vals) -> do
      evals <- mapM (\valTemp -> withBinding var valTemp (eval (Compr exp0 quals))) vals
      let finalVals = concatMap (\(ListVal x) -> x) evals
      return (ListVal finalVals)
    _ -> abort (EBadArg "bad arguments")
eval (Compr exp0 ((QIf exp):quals)) = do
  ifTrue <- eval exp
  case truthy ifTrue of
    True -> eval (Compr exp0 quals)
    False -> return (ListVal [])

--eval (Compr _ _) = abort (EBadArg "bad arguments")
exec :: Program -> Comp ()
exec [] = return ()
exec (SDef v exp:stmts) = do
  val <- eval exp
  withBinding v val (exec stmts)
exec (SExp exp:stmts) = do
  eval exp
  exec stmts

execute :: Program -> ([String], Maybe RunError)
execute p =
  case runComp (exec p) [] of
    (Left e, s) -> (s, Just e)
    (Right _, s) -> (s, Nothing)
