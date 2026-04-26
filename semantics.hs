module Semantics where
import SyntaxProject

evaluate :: Program -> IO Env
evaluate p = evaluateProg p []

evaluateProg :: [Stmt] -> Env -> IO Env
evaluateProg [] env = pure env
evaluateProg (s : ss) env = do
                             env' <- evaluateStmt s env
                             evaluateProg ss env'

                    
evaluateStmt :: Stmt -> Env -> IO Env
evaluateStmt (Assign v o) env = pure (updateEnv v (evaluateOper o env) env)

evaluateStmt (InputAssign v) env = do
                                putStr "Enter value (t/f): "
                                inputval <- getLine
                                case inputval of
                                    "t" -> pure (updateEnv v True env)
                                    "f" -> pure (updateEnv v False env)
                                    _ -> evaluateStmt (InputAssign v) env

evaluateStmt (Print o) env = do
                                print (evaluateOper o env)
                                pure env


evaluateOper :: Oper -> Env -> Bool
evaluateOper (And e1 e2) env = evaluateExpr e1 env && evaluateExpr e2 env

evaluateOper (Or e1 e2) env = evaluateExpr e1 env || evaluateExpr e2 env

evaluateOper (Nand e1 e2) env = not (evaluateExpr e1 env && evaluateExpr e2 env)

evaluateOper (Nor e1 e2) env = not (evaluateExpr e1 env || evaluateExpr e2 env)

evaluateOper (Xor e1 e2) env = evaluateExpr e1 env /= evaluateExpr e2 env
                            
evaluateOper (Xnor e1 e2) env = evaluateExpr e1 env == evaluateExpr e2 env

evaluateOper (Ex e) env = evaluateExpr e env

evaluateExpr :: Expr -> Env -> Bool
evaluateExpr (Paren o) env = evaluateOper o env

evaluateExpr (Not e) env = not (evaluateExpr e env)

evaluateExpr (BoolValue b) _ = b

evaluateExpr (Variable v) env =
    case lookup v env of
        Just value -> value
        Nothing -> error (show v ++ " has no value")

updateEnv :: Var -> Bool -> Env -> Env
updateEnv v value [] = [(v, value)]
updateEnv v value ((name, oldValue) : restEnv)
    | v == name = (name, value) : restEnv
    | otherwise = (name, oldValue) : updateEnv v value restEnv