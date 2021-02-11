module ElaboratorImpl where

import Absyn

import Control.Monad
import qualified Data.Map as M
import Data.Char

type CompTracker = [CName]
type ResTracker = [(RName, RName)]
type WriteData = [(CName, RProf)]
type StateData = DB
type ErrorData = String

-- monad ----------------------------------------

newtype RWSE a = RWSE { runRWSE :: ResTracker -> CompTracker -> StateData -> 
    Either ErrorData (a, WriteData, StateData) } 

instance Monad RWSE where
    return a = RWSE (\_ _ s -> Right (a, mempty, s))
    m >>= f = RWSE (\r c s -> case runRWSE m r c s of
        Left e -> Left e
        Right (a, w', s') -> case runRWSE (f a) r c s' of
            Left e -> Left e
            Right (b, w'', s'') -> Right (b, w'++ w'', s''))

instance Functor RWSE where
    fmap = liftM
instance Applicative RWSE where
    pure = return; (<*>) = ap 

-- monadic operations ---------------------------

withProvided :: [(RName, Int)] -> RWSE RProf -> RWSE RProf
withProvided [] _ = return []
withProvided [(name, n)] prof = RWSE (\r c s -> case lookup (map toUpper name) r of
    Nothing -> Left (name ++ " has not been declared")
    Just nameCap -> case runRWSE prof r c s of
        Right (profs, w, s) -> case M.lookup (R nameCap) (M.fromList profs) of
            Nothing -> Right ((R nameCap, (n, 0)):profs, w, s)
            Just (x, y) -> Right (M.toList (M.union (M.fromList [(R nameCap, (x+n, y))]) 
                (M.delete (R nameCap) (M.fromList profs))), w, s)
        Left e -> Left e)
withProvided ((name, n):specs) prof = withProvided [(name, n)] (withProvided specs prof)

withUsed :: [(RName, Int)] -> RWSE RProf -> RWSE RProf
withUsed [] _ = return []
withUsed [(name, n)] prof = RWSE (\r c s -> case lookup (map toUpper name) r of
    Nothing -> Left (name ++ " has not been declared")
    Just nameCap -> case runRWSE prof r c s of
        Right (profs, w, s) -> case M.lookup (R nameCap) (M.fromList profs) of
            Nothing -> Right ((R nameCap, (n*(-1), 0)):profs, w, s)
            Just (x, y) -> Right (M.toList (M.union (M.fromList [(R nameCap, (x-n, y))]) 
                (M.delete (R nameCap) (M.fromList profs))), w, s)
        Left e -> Left e)
withUsed ((name, n):specs) prof = withUsed [(name, n)] (withUsed specs prof)

withRequired :: [(RName, Int)] -> RWSE RProf -> RWSE RProf
withRequired [] _ = return []
withRequired [(name, n)] prof = RWSE (\r c s -> case lookup (map toUpper name) r of
    Nothing -> Left (name ++ " has not been declared")
    Just nameCap -> case runRWSE prof r c s of
        Right (profs, w, s) -> case M.lookup (R nameCap) (M.fromList profs) of
            Nothing -> Right ((R nameCap, (0, n)):profs, w, s)
            Just (x, y) -> Right (M.toList (M.union (M.fromList [(R nameCap, (x, y+n))]) 
                (M.delete (R nameCap) (M.fromList profs))), w, s)
        Left e -> Left e)
withRequired ((name, n):specs) prof = withRequired [(name, n)] (withRequired specs prof)

withNum :: Int -> [(RName, Int)] -> RWSE [(RName, Int)]
withNum _ [] = return []
withNum n ((name, x):specs) = do 
    let new = (name, n * x)
    newSpecs <- withNum n specs
    return (new : newSpecs)

withAnd :: [(RName, Int)] -> [(RName, Int)] -> RWSE [(RName, Int)]
withAnd and1 and2 = return (and1 ++ and2)

withResource :: Resource -> RWSE a -> RWSE a
withResource res rwse = let (R name) = res in
    RWSE (\r c s -> case lookup (map toUpper name) r of
        Nothing -> case runRWSE rwse ((map toUpper name, name):r) c s of
            Left e -> Left e
            Right (a, w, (rs, cs)) -> Right (a, w, (res:rs, cs))
        Just resource -> Left (resource ++ " has already been declared"))

withComp :: (CName, RProf) -> RWSE a -> RWSE a
withComp (cName, profile) rwse = RWSE (\r c s -> case lookup (map toUpper cName) r of
    Just resource -> Left (resource ++ " is declared as a resource")
    Nothing -> case runRWSE rwse r c s of
        Left e -> Left e
        Right (a, w, (rs, cs)) -> case lookup cName cs of
            Nothing -> case runRWSE rwse r (map toUpper cName:c) s of
                Left e -> Left e
                Right (a, w, (rs, cs)) -> if map toUpper cName `elem` c then
                    Left (cName ++ " has been declared in a different form") else
                        Right (a, w, (rs, (cName, profile):cs))
            Just _ -> Right (a, w, (rs, M.toList (M.adjust (makeNewProfile profile) 
                cName (M.fromList cs)))))

-- helpers --------------------------------------

makeNewProfile :: RProf -> RProf -> RProf
makeNewProfile [] profiles = profiles
makeNewProfile ((resource, (x, y)):profs) profiles = case M.lookup resource 
    (M.fromList profiles) of
        Nothing -> (resource, (x, y)) : makeNewProfile profs profiles
        Just (a, b) -> if x+a == 0 && y+b == 0 then
            makeNewProfile profs (M.toList (M.delete resource (M.fromList profiles))) 
                else 
                (resource, (x+a, y+b)) : makeNewProfile profs (M.toList 
                    (M.delete resource (M.fromList profiles)))

-- functions for elaborator ---------------------

evalSpec :: RSpec -> RWSE [(RName, Int)]
evalSpec (RSRes name) = return [(name, 1)]
evalSpec (RSNum x rspec) = do 
    let n = x
    spec <- evalSpec rspec
    withNum n spec
evalSpec (RSAnd rspec1 rspec2) = do
    spec1 <- evalSpec rspec1
    spec2 <- evalSpec rspec2
    withAnd spec1 spec2
evalSpec (RSOr rspec1 rspec2) = return [("#", 1)]

makeProfile :: [Clause] -> RWSE RProf
makeProfile [] = return []
makeProfile ((CKProvides, rspec):clauses) = do
    specification <- evalSpec rspec
    withProvided specification (makeProfile clauses)
makeProfile ((CKUses, rspec):clauses) = do
    specification <- evalSpec rspec
    withUsed specification (makeProfile clauses)
makeProfile ((CKRequires, rspec):clauses) = do
    specification <- evalSpec rspec
    withRequired specification (makeProfile clauses)

makeComp :: IComp -> RWSE (CName, RProf)
makeComp (IC cName clauses) = do
    profile <- makeProfile clauses
    return (cName, profile)

makeResource :: RName -> RWSE Resource
makeResource rname = return (R rname)

runState :: IDB -> RWSE ()
runState ([], []) = return ()
runState (rname:rnames, icomps) = do
    resource <- makeResource rname
    withResource resource (runState (rnames, icomps))
runState ([], icomp:icomps) = do
    comp <- makeComp icomp
    withComp comp (runState ([], icomps))


-- module functions -----------------------------

lookres :: [Resource] -> RName -> Either ErrMsg Resource
lookres [] _ = Left "Not found"
lookres (r:rs) rname = let (R name) = r in
    if map toUpper name == map toUpper rname then
        Right r else
            lookres rs rname

elaborate :: IDB -> Either ErrMsg DB
elaborate idb = case runRWSE (runState idb) mempty mempty ([], []) of
    Left e -> Left e
    Right (_, _, db) -> Right db
