-- Rudimentary test suite. Feel free to replace anything.

import Absyn
import Parser
import Elaborator
import Solver

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Tests" [
  testCase "parser" $
    parseString dbt @?= Right dbi,
  testCase "parser name too long" $
    parseString "component ccccccccccccccccccccccccccccccccc : uses r." @?= Left "Syntax error in input string",
  testCase "parser name ok" $
    parseString "component cccccccccccccccccccccccccccccccc : uses r." @?= Right ([],[IC "cccccccccccccccccccccccccccccccc" [(CKUses,RSRes "r")]]),
  testCase "parser num too long" $
    parseString "component c : uses 1000000 r." @?= Left "Syntax error in input string",
  testCase "parser num ok" $
    parseString "component c : uses 999999 r." @?= Right ([],[IC "c" [(CKUses,RSNum 999999 (RSRes "r"))]]),
    testCase "parser multiple resources & components" $
    parseString "resource r1, r2. component c1: provides r1. resource r3. component c2: uses r2; requires r3." @?= Right (["r1", "r2", "r3"], [IC "c1" [(CKProvides, RSRes "r1")], IC "c2" [(CKUses, RSRes "r2"), (CKRequires, RSRes "r3")]]),
  testCase "parser comments" $
    parseString "resource r. component c: {comment here} provides r." @?= Right dbi,
  testCase "parser nested comments" $
    parseString "resource r. {hello{my{friends}}}component c: provides r." @?= Right dbi,
  testCase "parser comments as white space" $
    parseString "resource{white}r. component c: provides r." @?= Right dbi,
  testCase "parser lots of white space" $
    parseString "resource   r . {}{}component c   : provides r ." @?= Right dbi,
  testCase "parser disambiguation 1" $
    parseString "component c: provides r1, 3 r2." @?= Right ([], [IC "c" [(CKProvides, RSAnd (RSRes "r1") (RSNum 3 (RSRes "r2")))]]),
  testCase "parser disambiguation 2" $
    parseString "component c: uses 4 r2 | r1, 3 r2." @?= Right ([], [IC "c" [(CKUses, RSOr (RSNum 4 (RSRes "r2")) (RSAnd (RSRes "r1") (RSNum 3 (RSRes "r2"))))]]),
  testCase "parser disambiguation with white space" $
    parseString "component c: uses 4{}r2   |{{}}r1, 3    r2  ." @?= Right ([], [IC "c" [(CKUses, RSOr (RSNum 4 (RSRes "r2")) (RSAnd (RSRes "r1") (RSNum 3 (RSRes "r2"))))]]),
  testCase "parser bad comment" $
    parseString "resource r.{hello{hey}component c: provides r." @?= Left "Syntax error in input string",
  testCase "parser bad input" $
    parseString "resource r. component c: i provides r" @?= Left "Syntax error in input string",
  testCase "parser parentheses" $
    parseString "component c: provides 2 (3 (r1|r2), 4 r3)." @?= Right ([], [IC "c" [(CKProvides, RSNum 2 (RSAnd (RSNum 3 (RSOr (RSRes "r1") (RSRes "r2"))) (RSNum 4 (RSRes "r3"))))]]),

  testCase "elaborator" $
    elaborate dbi @?= Right dbf,
  testCase "elaborator dup resource" $
    elaborate (["r", "r"], [IC "c" [(CKProvides, RSRes "r")]]) @?= Left "r has already been declared",
  testCase "elaborator dup res caps" $
    elaborate (["r", "R"], [IC "c" [(CKProvides, RSRes "r")]]) @?= Left "r has already been declared",
  testCase "elaborator canonically capped" $
    elaborate (["Resource"], [IC "c" [(CKProvides, RSRes "rEsOURcE")]]) @?= Right ([R "Resource"], [("c", [(R "Resource", (1,0))])]),
  testCase "elaborator many" $
    elaborate (["r1", "r2", "r3"], [IC "c1" [(CKProvides, RSAnd (RSRes "r1") (RSNum 3 (RSRes "r1"))), 
      (CKUses, RSRes "r2"), (CKRequires, RSNum 4 (RSRes "r2"))], IC "c2" [(CKProvides, RSAnd (RSRes "r3") 
      (RSNum 3 (RSRes "r1"))), (CKUses, RSNum 7 (RSRes "r1")), (CKRequires, RSAnd (RSNum 4 (RSRes "r2")) (RSRes "r1"))]]) 
        @?= Right ([R "r1",R "r2",R "r3"],[("c1",[(R "r1",(4,0)),
          (R "r2",(-1,4))]),("c2",[(R "r3",(1,0)),(R "r1",(-4,1)),(R "r2",(0,4))])]),
  testCase "elaborator fail combine" $
    elaborate (["r1", "r2", "r3"], [IC "c1" [(CKProvides, RSAnd (RSRes "r1") (RSNum 3 (RSRes "r1"))), 
      (CKUses, RSRes "r2"), (CKRequires, RSNum 4 (RSRes "r2"))], IC "C1" [(CKProvides, RSAnd (RSRes "r3") 
      (RSNum 3 (RSRes "r1"))), (CKUses, RSNum 7 (RSRes "r1")), (CKRequires, RSAnd (RSNum 4 (RSRes "r2")) (RSRes "r1"))]]) 
        @?= Left "C1 has been declared in a different form",
  testCase "elaborator combine correctly" $
    elaborate (["r1", "r2", "r3"], [IC "c1" [(CKProvides, RSAnd (RSRes "r1") (RSNum 3 (RSRes "r1"))), 
      (CKUses, RSRes "r2"), (CKRequires, RSNum 4 (RSRes "r2"))], IC "c1" [(CKProvides, RSAnd (RSRes "r3") 
      (RSNum 3 (RSRes "r1"))), (CKUses, RSNum 7 (RSRes "r1")), (CKRequires, RSAnd (RSNum 4 (RSRes "r2")) (RSRes "r1"))]]) 
        @?= Right ([R "r1",R "r2",R "r3"],[("c1",[(R "r1",(0,1)),
          (R "r2",(-1,8)), (R "r3",(1,0))])]),
  testCase "elaborator combine and remove" $
  elaborate (["r1", "r2", "r3"], [IC "c1" [(CKProvides, RSAnd (RSRes "r1") (RSNum 3 (RSRes "r1"))), 
    (CKUses, RSRes "r2")], IC "c1" [(CKProvides, RSRes "r2"), (CKUses, RSNum 4 (RSRes "r1"))]]) 
      @?= Right ([R "r1",R "r2",R "r3"],[("c1",[])]),
  testCase "elaborator" $
    elaborate (["r"], [IC "c1" [(CKProvides, RSRes "r")], IC "C1" [(CKProvides, RSRes "r")]]) 
      @?= Left "C1 has been declared in a different form",

  testCase "veifier just solved" $
    verify ([R "r"], [("c", [(R "r", (4, 2))])]) [(R "r", (0, 4))] [("c", 1)] @?= Right [(R "r",(4,4))],
  testCase "veifier with spare" $
    verify ([R "r"], [("c", [(R "r", (4, 2))])]) [(R "r", (0, 4))] [("c", 2)] @?= Right [(R "r",(8,4))],
  testCase "veifier failed" $
    verify ([R "r"], [("c", [(R "r", (1, 2))])]) [(R "r", (0, 4))] [("c", 2)] @?= Left "Not a valid solution"
  ]
  where
    dbt = "resource r. component c: provides r."
    dbi = (["r"], [IC "c" [(CKProvides, RSRes "r")]])
    dbf = ([R "r"], [("c", [(R "r", (1,0))])])
    goal = [(R "r", (0,1))]
    sol = [("c", 1)] 