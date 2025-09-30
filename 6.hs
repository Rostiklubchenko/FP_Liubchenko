module LambdaY where

data Expr
    = Var String
    | Lam String Expr
    | App Expr Expr
    | Let String Expr Expr
    | LetRec String Expr Expr
    deriving (Eq, Show)

eliminateRec :: Expr -> Expr
eliminateRec (Var v) = Var v
eliminateRec (Lam x e) = Lam x (eliminateRec e)
eliminateRec (App a b) = App (eliminateRec a) (eliminateRec b)
eliminateRec (Let x e body) = Let x (eliminateRec e) (eliminateRec body)
eliminateRec (LetRec f e body) =
    let e' = eliminateRec e
            body' = eliminateRec body
            yf = App (Var "Y") (Lam f e')
    in Let f yf body'

pretty :: Expr -> String
pretty = go 0
    where
        paren s = "(" ++ s ++ ")"
        go _ (Var v) = v
        go p (Lam x e) = paren $ "\\" ++ x ++ ". " ++ go 0 e
        go p (App a b) =
            let sa = go 1 a
                    sb = go 2 b
                    s = sa ++ " " ++ sb
            in if p>1 then paren s else s
        go _ (Let x e body) = "let " ++ x ++ " = " ++ go 0 e ++ " in " ++ go 0 body
        go _ (LetRec x e body) = "letrec " ++ x ++ " = " ++ go 0 e ++ " in " ++ go 0 body

factRec :: Expr
factRec =
    LetRec "fact" (Lam "n" body) (Var "fact")
    where
        body = App (App (App (Var "if") (App (Var "isZero") (Var "n"))) (Var "1"))
                             (App (App (Var "*") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n"))))

factNonRec :: Expr
factNonRec = eliminateRec factRec

mutualRec :: Expr
mutualRec = LetRec "a" (Lam "x" (App (Var "a") (Var "x")))
                         (LetRec "b" (Lam "x" (Var "b")) (Var "a"))

mutualNonRec :: Expr
mutualNonRec = eliminateRec mutualRec

-- це зробив чат гпт і я поняття не маю шо тут робиться і як це працює і навіщо це треба мені в житті я просто хочу щоб воно працювало і я розумів шо воно робить і ващє це шось сложно тут точно шось треба проще якось зробити але я поки не знаю шо від мене треба і як це зробити
