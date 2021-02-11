module SolverImpl where

import Absyn

import Control.Monad
import qualified Data.Map as M

-- monad ----------------------------------------

newtype Verifier a = Verifier { runVerifier :: DB -> Goal ->
    Either ErrMsg (a, Goal) }

instance Monad Verifier where
    return a = Verifier (\_ g -> Right (a, g))
    m >>= f = Verifier (\db g -> case runVerifier m db g of
        Left e -> Left e
        Right (a, g') -> case runVerifier (f a) db g' of
            Left e -> Left e
            Right (b, g'') -> Right (b, g''))
 
instance Functor Verifier where
    fmap = liftM
instance Applicative Verifier where
    pure = return; (<*>) = ap
    
-- verify ---------------------------------------

withInt :: Int -> (Resource, (Int, Int)) -> (Resource, (Int, Int))
withInt n (r, (x, y)) = (r, (x*n, y))

withAdded :: RProf -> Int -> Verifier () -> Verifier ()
withAdded profs x veri = Verifier (\db g -> case runVerifier veri db g of
    Left e -> Left e
    Right (a, g) -> Right (a, combine (map (withInt x) profs) g))

fromSol :: CName -> Verifier RProf
fromSol name = Verifier (\(_, cs) g -> case lookup name cs of
    Nothing -> Left (name ++ " is not a component")
    Just profile -> Right (profile, g))

checker :: Solution -> Verifier ()
checker [] = return ()
checker ((cname, x):sols) = do
    addToGoal <- fromSol cname
    withAdded addToGoal x (checker sols)

-- module functions -----------------------------

combine :: RProf -> RProf -> RProf
combine [] profiles = profiles
combine ((resource, (x, y)):profs) profiles = case M.lookup resource 
    (M.fromList profiles) of
        Nothing -> (resource, (x, y)) : combine profs profiles
        Just (a, b) | y > b -> (resource, (x + a, y)) : combine profs
                        (M.toList (M.delete resource (M.fromList profiles)))
                    | x + a == 0 && b == 0 -> combine profs
                        (M.toList (M.delete resource (M.fromList profiles)))
                    | otherwise -> (resource, (x + a, b)) : combine profs
                        (M.toList (M.delete resource (M.fromList profiles)))

checkGoal :: RProf -> Bool
checkGoal [] = True
checkGoal ((_, (x, y)):prof) = x >= y && checkGoal prof

verify :: DB -> Goal -> Solution -> Either ErrMsg RProf
verify database goal solution = case runVerifier (checker solution) database goal of
    Left e -> Left e
    Right (_, endGoal) -> if checkGoal endGoal then
        Right endGoal else
            Left "Not a valid solution"


solve :: DB -> Goal -> Int -> Either ErrMsg Solution
solve = undefined
