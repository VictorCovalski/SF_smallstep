import Estado
--ghci SmallStep
-- Author: Victor Renan Covalsi junes

--data F = Skip | Throw

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
        | Or  BExp BExp
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



interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

aSmallStep :: (AExp,Estado) -> (AExp,Estado)
--Var
aSmallStep (Var x,s) = (Num (procuraVar s x),s)

--Soma
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
					in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
					in (Som ef e2,s)

--Multiplicação
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y),s)
aSmallStep (Mul (Num x) e2,s) = let (ef,_) = aSmallStep(e2,s)
					in (Mul (Num x) ef,s)
aSmallStep (Mul e1 e2,s) = let (ef,_) = aSmallStep(e1,s)
					in (Mul ef e2,s)


interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False


bSmallStep :: (BExp,Estado) -> (BExp,Estado)
--Not
bSmallStep (Not FALSE,s) 	= (TRUE,s)
bSmallStep (Not TRUE,s) 	= (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
					in (Not bn ,sn)

--And
bSmallStep (And TRUE b2,s)	= (b2,s)
bSmallStep (And FALSE b2,s)	= (FALSE,s)
bSmallStep (And b1 b2,s)	= let (bn,sn) = bSmallStep (b1,s)
					in (And bn b2,sn)
--Or
bSmallStep (Or TRUE b2,s) = (TRUE,s)
bSmallStep (Or FALSE b2,s) = (b2,s)
bSmallStep (Or b1 b2,s) = let (bn,sn) = bSmallStep (b1,s)
					in (Or bn b2,sn)
--Igual
bSmallStep (Ig (Num x) (Num y),s) = if (x==y) then (TRUE,s) else (FALSE,s)
bSmallStep (Ig (Num x) e2,s) = let (ef,sn) = aSmallStep(e2,s)
					in (Ig (Num x) ef,sn)
bSmallStep (Ig e1 e2,s) = let(e1f,sn) = aSmallStep(e1,s)
					in (Ig e1f e2,sn)


interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (b,s) = if isFinalC b then (b,s) else interpretC (cSmallStep (b,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC _ = False

cSmallStep :: (CExp,Estado) -> (CExp,Estado)
--While
cSmallStep (While b c,s) = (If b (Seq c (While b c)) Skip, s)

--If
cSmallStep (If FALSE c1 c2,s) = (c2,s)
cSmallStep (If TRUE c1 c2 ,s) = (c1,s)
cSmallStep (If b c1 c2, s) 	  = let (b2,s2) = bSmallStep(b,s)
					in (If b2 c1 c2, s)
-- Try catch					
cSmallStep (Try Skip c2,s) = (Skip,s)
cSmallStep (Try Throw c2,s) = (c2,s)

-- Seq
cSmallStep (Seq Skip c,s) = (c,s)
cSmallStep (Seq Throw c,s) = (Throw,s)
cSmallStep (Seq c1 c2,s) = let(cf,s1) = cSmallStep(c1,s)
					in (Seq cf c2,s)
-- Atrib
cSmallStep (Atrib (Var x) (Num y),s) = let(sf) = (mudaVar s x y)
								in (Skip,sf)	
cSmallStep (Atrib (Var x) e,s) = let(ef,sf) = aSmallStep(e,s)
					in (Atrib (Var x) ef,sf)

-- interpretC :: (CExp,Estado) -> (CExp,Estado)
-- interpretC (b,s) = ?

-- isFinalC :: CExp -> Bool

-- csmallStep :: (CExp,Estado) -> (CExp,Estado)



meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

exemplo3 :: CExp
exemplo3 = (While (Ig (Var "x") (Num 3)) (Atrib (Var "x") (Som (Var "x") (Num 1))))