-- Rudimentary test suite. Feel free to replace anything.

import Ast
import Data
import Parser
import Renderer

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Tests" [
    testCase "parser" $
        parseString tms @?= Right tmp,
    testCase "condition" $
        parseString test_1 @?= Right check_1,
    testCase "parserEx" $
        parseString slurEx @?= Right tempEx,
    testCase "capture" $
        parseString "{% capture cap%} hello{% endcapture %}" @?= Right [TCapture "cap" [TLit " hello"]],
    testCase "many cond" $
        parseString "{% if x %}{% if var <= 3 + 1 %}4{%else%}{% endif%}{% elsif  y.field %}y has a field{%endif %}\n" @?=
            Right [TIf (EVar "x") [TIf (ELeq (EVar "var") (EPlus (ENum 3) (ENum 1))) [TLit "4"] []] 
                [TIf (EField (EVar "y") "field") [TLit "y has a field"] []],TLit "\n"],
    testCase "cond fail" $
        parseString "{% if x %}{% if var <= 3 + 1 %}4{%else%}{% endif%}{% elsify.field %}y has a field{%endif %}\n" @?=
            Left "(line 1, column 59):\nunexpected \"y\"\nexpecting space or new-line",

    testCase "renderer" $
        render ctx tmp @?= Right out,
    testCase "rendererEx" $
        render ctxEx tempEx @?= Right outEx,
    testCase "render fail" $
        render [] [TIf (EVar "x") [TIf (ELeq (EVar "var") (EPlus (ENum 3) (ENum 1))) [TLit "4"] []] 
                [TIf (EField (EVar "y") "field") [TLit "y has a field"] []],TLit "\n"] @?=
                Left "Bad variable name" ]
    where
        tms = "Hello, {{user.first_name}}!\n"
        tmp = [TLit "Hello, ",
           TOutput (EField (EVar "user") "first_name"),
           TLit "!\n"]
        test_1 = "{% if x %} foo {% elsif y %} bar {% else %}{% endif %} baz"
        check_1 = [TIf (EVar "x") [TLit " foo "] [TIf (EVar "y") [TLit " bar "] []], TLit " baz"]
        ctx = [("user", R [("first_name", S "John"), ("last_name", S "Doe")])]
        out = "Hello, John!\n"
        slurEx = "{% if order.items %}You have ordered the following items:\n\
            \{% assign i = 0 %}{% for item in order.items %}{% assign i = i+1 %}\n\
            \  {{i}}. {{item.name}}{% if 2 <= item.count %} (quantity {{item.count}}){% endif %}\n\
            \{% endfor %}\n\
            \Thank you for shopping with us, {{order.client}}!\n\
            \{% else %}You haven't ordered anything yet!\n\
            \{% endif %}\n"
        tempEx = [TIf (EField (EVar "order") "items")
            [TLit "You have ordered the following items:\n",
            TAssign "i" (ENum 0),
            TFor "item" (EField (EVar "order") "items")
            [TAssign "i" (EPlus (EVar "i") (ENum 1)),
            TLit "\n  ", TOutput (EVar "i"), TLit ". ",
            TOutput (EField (EVar "item") "name"),
            TIf (ELeq (ENum 2) (EField (EVar "item") "count"))
            [TLit " (quantity ",
            TOutput (EField (EVar "item") "count"),
            TLit ")"]
            [ ],
            TLit "\n"],
            TLit "\nThank you for shopping with us, ",
            TOutput (EField (EVar "order") "client"),
            TLit "!\n"]
            [TLit "You haven't ordered anything yet!\n"],
            TLit "\n"]
        ctxEx =   [("order", R [("client", S "John Smith"),
            ("items", L [R [("name", S "Universal widget"),
            ("count", N 1)],
            R [("name", S "Small gadget"),
            ("count", N 10)]])])]
        outEx = "You have ordered the following items:\n\n  1. \
            \Universal widget\n\n  2. Small gadget (quantity 10)\n\nThank you for shopping with us, John Smith!\n\n"
