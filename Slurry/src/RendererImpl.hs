module RendererImpl (RenderErr, render) where

import Ast
import Data

import Control.Monad
import qualified Data.Map as M

type RenderErr = String

-- EvalM monad --------------
newtype EvalM a =  EvalM { evaluate :: Ctx -> 
        Either RenderErr a }

instance Monad EvalM where
    return a = EvalM (\_ -> Right a)
    m >>= f = EvalM (\c -> case evaluate m c of
        Left e -> Left e
        Right a -> case evaluate (f a) c of
            Left e -> Left e
            Right b -> Right b)

instance Functor EvalM where
    fmap = liftM
instance Applicative EvalM where
    pure = return; (<*>) = ap 

-- ExecM monad --------------
newtype ExecM a = ExecM { execute :: Ctx -> 
        Either RenderErr (a, String, Ctx) } 

instance Monad ExecM where
    return a = ExecM (\ctx -> Right (a, mempty, ctx))
    m >>= f = ExecM (\ctx -> case execute m ctx of
        Left e -> Left e
        Right (a, w', ctx') -> case execute (f a) ctx' of
            Left e -> Left e
            Right (b, w'', ctx'') -> Right (b, w'++ w'', ctx''))

instance Functor ExecM where
    fmap = liftM
instance Applicative ExecM where
    pure = return; (<*>) = ap
-----------------------------

-- evaluator functions ------
look :: Var -> EvalM Value
look var = EvalM (\ctx ->
    case lookup var ctx of
        Nothing -> Left "Bad variable name"
        Just x -> Right x)

field :: Var -> Value -> EvalM Value
field x (R ctx') = EvalM (\_ ->
    case lookup x ctx' of
        Nothing -> Left "Bad field name"
        Just x -> Right x)
field _ _ = EvalM (\_ -> Left "Not a record")

add :: Value -> Value -> EvalM Value
add (N n1) (N n2) = return (N (n1 + n2))
add _ _ = EvalM (\_ -> Left "You can only add numbers")

leq :: Value -> Value -> EvalM Value
leq (N n1) (N n2) = if n1 <= n2 then
    return (N 1) else return (N 0)
leq _ _ = EvalM (\_ -> Left "You can only compare numbers")

-- evaluator ----------------
eval :: Exp -> EvalM Value
eval (ENum n) = return (N n) 
eval (EVar x) = look x
eval (EField e x) = do
    exp <- eval e
    field x exp
eval (EPlus e1 e2) = do 
    exp1 <- eval e1
    exp2 <- eval e2
    add exp1 exp2
eval (ELeq e1 e2) = do 
    exp1 <- eval e1
    exp2 <- eval e2
    leq exp1 exp2

-- executor functions -------
extract :: EvalM a -> ExecM a
extract eval = ExecM (\ctx -> 
    case evaluate eval ctx of
        Left e -> Left e
        Right val -> Right (val, mempty, ctx))

throw :: String -> ExecM a
throw e = ExecM (\_ -> Left e)

tell :: String -> ExecM ()
tell s = ExecM (\ctx -> Right ((), s, ctx))

output :: Value -> ExecM ()
output (S s) = ExecM (\ctx -> Right ((), s, ctx))
output (N n) = ExecM (\ctx -> let num = show n in
    Right ((), num, ctx))
output _ = ExecM (\_ -> Left "You can only output numbers and strings")

withAssignment :: Var -> Value -> ExecM a -> ExecM a
withAssignment var val exec = ExecM (\ctx -> 
    case lookup var ctx of 
        Nothing -> execute exec ((var, val) : ctx)
        Just _ -> execute exec ((var, val) : M.toList 
                (M.delete var (M.fromList ctx))))

conditional :: Value -> Template -> Template -> ExecM ()
conditional (N 0) _ t2 = exec t2
conditional (N _) t1 _ = exec t1
conditional (L []) _ t2 = exec t2
conditional (L (_:_)) t1 _ = exec t1
conditional (S "") _ t2 = exec t2
conditional (S (_:_)) t1 _ = exec t1
conditional _ _ _ = throw "Only numbers can be conditions"

forLoop :: Var -> Value -> Template -> ExecM ()
forLoop _ (L []) _ = return ()
forLoop v (L (val:vals)) t = do 
    withAssignment v val (exec t)
    forLoop v (L vals) t
forLoop _ _ _ = throw "You can only iterate through a list"

capture :: Var -> Template -> ExecM ()
capture v t = ExecM (\ctx ->
    case execute (exec t) ctx of
        Left e -> Left e
        Right ((), s, ctx') -> case lookup v ctx' of 
            Nothing -> Right ((), mempty, (v, S s) : ctx')
            Just _ -> Right ((), mempty, (v, S s) : M.toList 
                    (M.delete v (M.fromList ctx'))))

-- executor -----------------
exec :: Template -> ExecM ()
exec [] = return ()
exec (TLit s:frags) = do
    tell s
    exec frags
exec (TOutput e:frags) = do
    val <- extract (eval e)
    output val
    exec frags
exec (TAssign v e:frags) = do
    val <- extract (eval e)
    withAssignment v val (exec frags)
exec (TIf e t1 t2:frags) = do
    val <- extract (eval e)
    conditional val t1 t2
    exec frags
exec (TFor v e t:frags) = do
    val <- extract (eval e)
    forLoop v val t 
    exec frags
exec (TCapture v t:frags) = do
    capture v t
    exec frags

-- module -------------------
render :: Ctx -> Template -> Either RenderErr String
render ctx t = case execute (exec t) ctx of
    Left e -> Left e
    Right ((), s, _) -> Right s