data Exp = Num Int | TRUE | FALSE | Var String | Soma Exp Exp | Sub Exp Exp | Div Exp Exp
	| Mult Exp Exp | Suc Exp | Pred Exp |And Exp Exp | Or Exp Exp | Not Exp | IF Exp Exp Exp
	| Ap Exp Exp | Fun String Tipo Exp | Let String Tipo Exp Exp
	deriving (Eq,Show)
data Tipo = INT | BOOL | F Tipo Tipo
	deriving (Eq, Show)


runFP :: Exp -> Exp
runFP exp = if isFinal exp then exp else runFP (smallStep exp)

isFinal :: Exp -> Bool
isFinal (Num n) = True
isFinal TRUE = True
isFinal FALSE = True
isFinal (Fun s t e) = True
isFinal _ = False


smallStep :: Exp -> Exp
-- Adição
smallStep (Soma (Num n1) (Num n2)) = Num (n1 + n2)
smallStep (Soma (Num n1) e2 ) = Soma (Num n1) (smallStep e2)
smallStep (Soma e1 e2) = Soma (smallStep e1) e2
-- Subtração
smallStep (Sub (Num n1) (Num n2)) = Num(n1-n2)
smallStep (Sub (Num n) e) = Mult (Num n) (smallStep e)
smallStep (Sub e1 e2) = Sub (smallStep e1) e2
-- Multiplicação
smallStep (Mult (Num n1) (Num n2)) = Num (n1*n2)
smallStep (Mult (Num n) e) = Mult (Num n) (smallStep e)
smallStep (Mult e1 e2) = Mult (smallStep e1) e2
-- Divisão
smallStep (Div (Num n1) (Num n2)) = Num (n1/n2)
smallStep (Div (Num n) e) = Mult (Num n) (smallStep e)
smallStep (Div e1 e2) = Mult (smallStep e1) e2
-- Sucessor
smallStep (Suc (Num n)) = Num (n+1)
smallStep (Suc e) = Suc (smallStep e)
-- Predecessor
smallStep (Pred (Num n)) = Num (n-1)
smallStep (Pred e) = Pred (smallStep e)
-- Not
smallStep (Not TRUE) = FALSE
smallStep (Not FALSE) = TRUE
smallStep (Not b) = Not(smallStep b)
--And
smallStep (And TRUE b) = b
smallStep (And FALSE b) = FALSE
smallStep (And b1 b2) = And (smallStep b1) b2  
--Or
smallStep (Or TRUE b) = TRUE
smallStep (Or FALSE b) = b
smallStep (Or b1 b2) = Or (smallStep b1) b2
--Less or equal
smallStep (Leq (Num n1) (Num n2))
		| n1 < n2 = TRUE
		| otherwise = FALSE
smallStep (Leq (Num n1) e) = Leq (Num n1) (smallStep e)
smallStep (Leq e1 e2) = Leq (smallStep e1) e2
-- Aplicação de função
smallStep (Ap (Fun var tipo e1) e2) = if(isFinal e2)
					then subs var e2 e1
					else Ap(Fun var tipo e1) (smallstep e2)
smallStep (Ap e1 e2) = (Ap (smallStep e1) e2)
-- Let
smallStep (Let 
--Substituicao utilizada em Let e Fun --construir arvore sintatica
subs :: String -> Exp -> Exp -> Exp
subs var val (Soma exp1 exp2) = Soma (subs var val exp1) (subs var val exp2)
subs 
subs var val (Sub exp1 exp2)  = Sub (subs var val exp1) (subs var val exp2)
-- subs var val ? = ?
-- {v/x}e1 = subs x v e1

progTeste :: Exp
progTeste = Soma (Soma (Num 4) (Num 5)) (Num 12)
 
prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2 
-- Resp: 3


prog2 :: Exp
prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11


prog3 :: Exp
prog3 = Fun "f1" (F INT INT) (Fun "y" BOOL (Soma (Num 4) (Ap (Var "f1") (Num 1))))

-- > fun f1 : Int -> Int => (fun y: Bool => 4 + (f1 1))

prog4 :: Exp
prog4 = Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Var "x") 

