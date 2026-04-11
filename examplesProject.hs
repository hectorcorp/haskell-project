import SyntaxProject

{-
To run in terminal:
ghci examplesProject.hs
:load examplesProject.hs
s1    s2    s3
main
-}


{- 
p1:
A = False
print A
C = True or (True nand !False)
-}

-- A = False
s1 :: Stmt
s1 = Assign A (Ex (BoolValue False))

-- print A
s2 :: Stmt
s2 = Print (Ex (Variable A))

-- C = True or (True nand !False)
s3 :: Stmt
s3 = Assign C(Or (BoolValue True)(Paren (Nand (BoolValue True) (Not (BoolValue False)))))




p1 :: Program
p1 = [s1, s2, s3]

main :: IO ()
main = print p1
