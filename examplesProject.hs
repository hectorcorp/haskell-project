import SyntaxProject

{-
To run in terminal:
ghci examplesProject.hs
-}

-- A = False
s1 :: Stmt
s1 = AssignF A (Ex (Bool (Val False)))

-- B = True OR A
s2 :: Stmt
s2 = AssignF B (Or (Bool (Val True)) (Variable A))

-- print B
s3 :: Stmt
s3 = Print (Ex (Variable B))

-- C = True OR (True NAND !False)
s4 :: Stmt
s4 = AssignF C(Or (Bool (Val True))(Paren (Nand (Bool (Val True)) (Not (Bool (Val False))))))

-- print C
s5 :: Stmt
s5 = Print (Ex (Variable C))

{-
A = False
B = True OR A
C = True OR (True NAND !False)
print C
-}
p1 :: Program
p1 = [s1, s2, s3, s4, s5]

