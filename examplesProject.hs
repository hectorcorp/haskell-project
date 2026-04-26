import SyntaxProject
import Semantics

{-
To run in terminal:
ghci examplesProject.hs
:load examplesProject.hs
s1    s2    s3
main
mapM_
pure ()                           to print just output of program
print finalEnv                    to print the final env as well
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

s4 :: Stmt
s4 = InputAssign D

s5 :: Stmt
s5 = Print (Ex (Variable D))

p1 :: Program
p1 = [s1, s2, s3, s4, s5]

printP :: Program -> IO ()
printP = putStrLn . printProgram

main :: IO ()
main = do
        finalEnv <- evaluate p1
        pure ()