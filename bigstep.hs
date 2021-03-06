import Estado


data AExp = 	Num Int
        |Var String
		|Som AExp AExp
        |Sub AExp AExp
		|Mul AExp AExp
              deriving(Show)

data BExp =	 TRUE
		| FALSE
        | Not BExp
		| And BExp BExp
        |Or  BExp BExp
		| Ig  AExp AExp
              deriving(Show)

data CExp =    While BExp CExp
		| If BExp CExp CExp
		| Seq CExp CExp
		| Atrib AExp AExp
		| Try CExp CExp
        | Skip 
        | Throw
	deriving(Show)                



abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s)  = let	(n1,s1) = abigStep (e1, s)
				(n2,s2) = abigStep (e2, s)
					in (n1+n2,s)

--abigStep (Sub e1 e2,s)  = let   (n1,s1) = abigStep (e1,s)
--				(n2,s2) = abigStep (e2,s)
--					in (n1-n2,s)

--abigStep (Mul e1 e2,s)  = let (n1,s1) = abigStep(e1,s)
--				(n2,s2) = abigStep (e2,s)
--					in (n1*n2,s)


bbigStep :: (BExp,Estado) -> (Bool,Estado)
bbigStep (TRUE,s)  	= (True,s)
bbigStep (FALSE,s) 	= (False,s)
bbigStep (Not b,s) 	= let 	(b1,s1) = bbigStep (b,s)
					in (not b1,s1) 
bbigStep (Ig e1 e2,s )  = let 	(n1,s1) = abigStep (e1,s)
			 	(n2,s2) = abigStep (e2,s)
					in (n1 == n2, s)

bbigStep (And b1 b2,s )  = let 	(b1f,s1) = bbigStep (b1,s)
			 	(b2f,s2) = bbigStep (b2,s)
					in case b1f of
					True -> (b2f,s)
					False -> (False,s)

bbigStep (Ig b1 b2,s )  = let 	(b1f,s1) = abigStep (b1,s)
			 	(b2f,s2) = abigStep (b2,s)
			 	in (b1f == b2f,s)

cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s)      	= (Skip,s)
cbigStep (Throw,s) 		= (Throw,s)
cbigStep (While b c, s) = let (b1,s1) = bbigStep (b,s)
				in case b1 of
					True -> let 	(_,s2) = cbigStep (c,s)
							(_,s3) = cbigStep (While b c,s2)
							in (Skip,s3)
					False -> (Skip,s)
cbigStep(Try c1 c2,s) = let (cf,s1) = cbigStep(c1,s)
				in case cf of
					Throw -> cbigStep(c2,s1)
					Skip -> (Skip,s1)

cbigStep (If b c1 c2,s) = let (b1,s1) = bbigStep(b,s) in
				if b1 then cbigStep(c1,s) else cbigStep(c2,s)

cbigStep (Seq c1 c2,s)  = let (cf,s1) = cbigStep(c1,s)
				in case cf of
					Throw -> (Throw,s1)
					Skip -> cbigStep(c2,s1)

cbigStep (Atrib (Var x) e,s) = let (y,c) = abigStep(e,s);
									(sf) = (mudaVar s x y)
									in (Skip,sf)




meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

exemplo:: CExp
exemplo = (Try (Seq (Atrib (Var "x") (Num 2))  (Seq Throw (Atrib (Var "x") (Num 3)))) (Atrib (Var "x") (Som (Var "x") (Var "x"))))

exemplosimples:: CExp
exemplosimples = (Try Throw (Atrib (Var "x") (Num 5)))


--exemplo :: AExp
--exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

--teste1 :: BExp
--teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
--teste2 :: BExp
--teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


--testec1 :: CExp
--testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
--		(Atrib (Var "y") (Var "z")))

--fatorial :: CExp
--fatorial = (Seq (Atrib (Var "y") (Num 1))
--                (While (Not (Ig (Var "x") (Num 1)))
--                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
--                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
