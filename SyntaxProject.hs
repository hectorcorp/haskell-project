{- HLINT ignore "Use newtype instead of data" -}
module SyntaxProject where

{-
Possible Strings

a = True OR False

b = True OR (True AND !(False))

print a

c = a OR b

print c

print (!False)

print !(False)
 
-}



{-
<program> -> <stmts>

<stmts> -> <stmt><stmts> | epsilon

<stmt> -> <var> = <oper> | print <oper> 

<oper> -> <expr> AND <expr> 
        | <expr> OR <expr> 
        | <expr> XNOR <expr> 
        | <expr> NAND <expr> 
        | <expr> NOR <expr> 
        | <expr>

<expr> -> (<oper>) | !<expr> | <bool> | <var>

<bool> -> True | False

<var> -> A | B | C | D | E | F 

<env> -> [(<var>, <oper>)]
-}

type Program = [Stmt]

data Stmt = AssignF Var Oper | Print Oper

instance Show Stmt where 
    show :: Stmt -> String
    show (AssignF v o) = show v ++ " = " ++ show o
    show (Print o) = "print " ++ show o

data Oper = And Expr Expr | Or Expr Expr | Xnor Expr Expr | Nand Expr Expr | Nor Expr Expr | Ex Expr

instance Show Oper where
    show (And e1 e2) = show e1 ++ " AND " ++ show e2
    show (Or e1 e2 ) = show e1 ++ " OR " ++ show e2
    show (Xnor e1 e2 ) = show e1 ++ " XNOR " ++ show e2
    show (Nand e1 e2 ) = show e1 ++ " NAND " ++ show e2
    show (Nor e1 e2 ) = show e1 ++ " NOR " ++ show e2
    show (Ex e1) = show e1

data Expr = Paren Oper | Not Expr | Bool BoolValue | Variable Var

instance Show Expr where
    show (Paren o) = "(" ++ show o ++ ")"
    show (Not e) = "!" ++ show e
    show (Bool v) = show v
    show (Variable v) = show v

data Var = A | B | C | D | E | F

instance Show Var where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"

data BoolValue = Val Bool

instance Show BoolValue where
    show (Val b) = show b

type Env = [(Var, Expr)]
