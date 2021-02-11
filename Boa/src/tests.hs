import BoaAST
import BoaInterp
import BoaParser
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "tests"
    [ testCase "" $ parseString "2" @?= Right [SExp (Const (IntVal 2))]
    , testCase "" $ parseString "10" @?= Right [SExp (Const (IntVal 10))]
    , testCase "" $ parseString "-10" @?= Right [SExp (Const (IntVal (-10)))]
    , testCase "" $ parseString "0" @?= Right [SExp (Const (IntVal 0))]
    , testCase "" $ parseString "'00'" @?= Right [SExp (Const (StringVal "00"))]
    , testCase "" $ parseString "'abc'" @?= Right [SExp (Const (StringVal "abc"))]
    , testCase "" $ parseString "'abc '" @?= Right [SExp (Const (StringVal "abc "))]
    , testCase "" $ parseString "_10" @?= Right [SExp (Var ("_10"))]
    , testCase "" $ parseString "A_10" @?= Right [SExp (Var ("A_10"))]
    , testCase "" $ parseString "_10" @?= Right [SExp (Var ("_10"))]
    , testCase "" $ parseString "a=true" @?= Right [SDef "a" (Var "true")]
    , testCase "" $ parseString "a=[12]" @?= Right [SDef "a" (List [Const (IntVal 12)])]
    , testCase "" $ parseString "a =[12]" @?= Right [SDef "a" (List [Const (IntVal 12)])]
    , testCase "" $ parseString "a=12" @?= Right [SDef "a" (Const (IntVal 12))]
    , testCase "" $
      parseString "'nik\\'b\\\\c\\nd'" @?= Right [SExp (Const (StringVal "nik'b\\c\nd"))]
    , testCase "" $
      parseString "a =12; a+12" @?=
      Right [SDef "a" (Const (IntVal 12)), SExp (Oper Plus (Var "a") (Const (IntVal 12)))]
    , testCase "" $
      parseString "a =12; a-12" @?=
      Right [SDef "a" (Const (IntVal 12)), SExp (Oper Minus (Var "a") (Const (IntVal 12)))]
    , testCase "" $
      parseString "a =12; 12*2" @?=
      Right [SDef "a" (Const (IntVal 12)), SExp (Oper Times (Const (IntVal 12)) (Const (IntVal 2)))]
    , testCase "" $
      parseString "a =12; 12%2" @?=
      Right [SDef "a" (Const (IntVal 12)), SExp (Oper Mod (Const (IntVal 12)) (Const (IntVal 2)))]
    , testCase "" $
      parseString "12 > 2" @?= Right [SExp (Oper Greater (Const (IntVal 12)) (Const (IntVal 2)))]
    , testCase "" $
      parseString "2 in [1,2,3]" @?=
      Right
        [ SExp
            (Oper
               In
               (Const (IntVal 2))
               (List [Const (IntVal 1), Const (IntVal 2), Const (IntVal 3)]))
        ]
    , testCase "" $
      parseString "a =[12,12]" @?=
      Right [SDef "a" (List [(Const (IntVal 12)), (Const (IntVal 12))])]
    , testCase "" $
      parseString "x #asdasdasdad \n; 12 #adfadsf \n; 'ab'" @?=
      Right [SExp (Var "x"), SExp (Const (IntVal 12)), SExp (Const (StringVal "ab"))]
    , testCase "" $
      parseString "1 != 3" @?= Right [SExp (Not (Oper Eq (Const (IntVal 1)) (Const (IntVal 3))))]
    , testCase "" $
      parseString "1 == 3" @?= Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 3)))]
    , testCase "" $ parseString "x #afdafda " @?= Right [SExp (Var "x")]
    , testCase "" $
      parseString "a = [12,12]" @?=
      Right [SDef "a" (List [(Const (IntVal 12)), (Const (IntVal 12))])]
    , testCase "" $
      parseString "a= [12,12]" @?=
      Right [SDef "a" (List [(Const (IntVal 12)), (Const (IntVal 12))])]
    , testCase "" $
      parseString "a=12; b=[1123]; 'hi'" @?=
      Right
        [ SDef "a" (Const (IntVal 12))
        , SDef "b" (List [Const (IntVal 1123)])
        , SExp (Const (StringVal "hi"))
        ]
    , testCase "" $
      parseString "a=12; b=[1123]; 'hi'" @?=
      Right
        [ SDef "a" (Const (IntVal 12))
        , SDef "b" (List [Const (IntVal 1123)])
        , SExp (Const (StringVal "hi"))
        ]
    , testCase "" $
      parseString "[1+2, 2+2, 2-2]" @?=
      Right
        [ SExp
            (List
               [ Oper Plus (Const (IntVal 1)) (Const (IntVal 2))
               , Oper Plus (Const (IntVal 2)) (Const (IntVal 2))
               , Oper Minus (Const (IntVal 2)) (Const (IntVal 2))
               ])
        ]
    , testCase "" $
      parseString "[x for x in [1,2,3]]" @?=
      Right
        [ SExp
            (Compr
               (Var "x")
               [QFor "x" (List [Const (IntVal 1), Const (IntVal 2), Const (IntVal 3)])])
        ]
    , testCase "" $ error "001"
    , testCase "" $ error "'0\0'"
    , testCase "" $ error "00"
    , testCase "" $ parseString "not#foo\nx" @?= Right [SExp (Not (Var "x"))]
    , testCase "" $ error "-00"
    , testCase "" $ error "0'0"
    , testCase "" $ error "if"
    , testCase "" $ error "'''"
    , testCase "" $ error "'a'a'"
    , testCase "" $ error "not"
    , testCase "" $ error "2in[1,2,3]"
    , testCase "" $ error "'a0'= 12"
    , testCase "" $ error "'a0' + 'asd'"
    , testCase "" $ error "'a0' in 'asd'"
    , testCase "" $ error "'a0' * 'asd'"
    , testCase "" $ error "'a0' % 'asd'"
    , testCase "" $ error "'a0' > 'asd'"
    , testCase "" $ error "'a0' // 'asd'"
    , testCase "" $ error "'a0' != 'asd'"
    , testCase "" $ error "'a0' == 'asd'"
    , testCase "" $ error "a=12; b[1123]; 'hi'"
    , testCase "" $ error "a=12; b=[1123]; 'hi"
    , testCase "" $ error "a=12; b=[1123]; 'hi';"
    , testCase "" $ error "'a'=12; b=[1123]; 'hi';"
    , testCase "" $ error "a!=12; b=[1123]; 'hi';"
    , testCase "" $ error "[x for x in [\"a\",\"a\",3]]"
    ]
  where
    error string =
      case parseString string of
        Left e -> return () -- any message is OK
        Right p -> assertFailure $ "Unexpected parse: " ++ show p
