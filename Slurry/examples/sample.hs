  [TIf (EField (EVar "order") "items")
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
