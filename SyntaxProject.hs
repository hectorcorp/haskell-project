module SyntaxProject where

{-
Possible Strings

A = True or False
B = True or (True and !(False))
print A
C = A or B
print C

Same freedom as cpp !
print (!!!False)

Same freedom as cpp ()
print !(((False)))



Not possible strings

Need to force parathesis with multiple operations
a = True OR False AND True
-}



{-  Syntax Free Grammer


<program> -> <stmts>

<stmts> -> <stmt><stmts> | epsilon

<stmt> -> <var> = <oper> | <var> = input() | print <oper> 

<oper> -> <expr> and <expr> 
        | <expr> or <expr> 
        | <expr> nand <expr> 
        | <expr> nor <expr> 
        | <expr> xor <expr>
        | <expr> xnor <expr>
        | <expr>

<expr> -> (<oper>) | !<expr> | <bool> | <var>

<bool> -> True | False

<var> -> A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

<env> -> [(<var>, <bool>)]
-}

type Program = [Stmt]

printProgram :: Program -> String
printProgram [] = ""
printProgram [s] = show s ++ "\n"
printProgram (s:ss) = show s ++ "\n" ++ printProgram ss

data Stmt = Assign Var Oper | InputAssign Var | Print Oper

instance Show Stmt where 
    show (Assign v o) = show v ++ " = " ++ show o
    show (InputAssign v) = show v ++ " = input()"
    show (Print o) = "print " ++ show o

data Oper =   And Expr Expr 
            | Or Expr Expr 
            | Nand Expr Expr 
            | Nor Expr Expr 
            | Xor Expr Expr 
            | Xnor Expr Expr
            | Ex Expr

instance Show Oper where
    show (And e1 e2) = show e1 ++ " and " ++ show e2
    show (Or e1 e2 ) = show e1 ++ " or " ++ show e2
    show (Nand e1 e2 ) = show e1 ++ " nand " ++ show e2
    show (Nor e1 e2 ) = show e1 ++ " nor " ++ show e2
    show (Xor e1 e2 ) = show e1 ++ " xor " ++ show e2
    show (Xnor e1 e2 ) = show e1 ++ " xnor " ++ show e2
    show (Ex e1) = show e1

data Expr = Paren Oper | Not Expr | BoolValue Bool | Variable Var 

instance Show Expr where
    show (Paren o) = "(" ++ show o ++ ")"
    show (Not e) = "!" ++ show e
    show (BoolValue b) = show b
    show (Variable v) = show v

data Var = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Show, Eq)

type Env = [(Var, Bool)]

