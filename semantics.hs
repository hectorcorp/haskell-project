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
evaluateStmt (Assign v o) env = do
                                 value <- evaluateOper o env
                                 pure (updateEnv v value env)

evaluateStmt (Print o) env = do
                                value <- evaluateOper o env
                                print value
                                pure env


evaluateOper :: Oper -> Env -> IO Bool
evaluateOper (And e1 e2) env = do
                                value1 <- evaluateExpr e1 env
                                value2 <- evaluateExpr e2 env
                                pure (value1 && value2)

evaluateOper (Or e1 e2) env = do
                                value1 <- evaluateExpr e1 env
                                value2 <- evaluateExpr e2 env
                                pure (value1 || value2)

evaluateOper (Nand e1 e2) env = do
                                value1 <- evaluateExpr e1 env
                                value2 <- evaluateExpr e2 env
                                pure (not (value1 && value2))

evaluateOper (Nor e1 e2) env = do
                                value1 <- evaluateExpr e1 env
                                value2 <- evaluateExpr e2 env
                                pure (not (value1 || value2))

evaluateOper (Xor e1 e2) env = do
                                value1 <- evaluateExpr e1 env
                                value2 <- evaluateExpr e2 env
                                pure (value1 /= value2)
                            
evaluateOper (Xnor e1 e2) env = do
                                value1 <- evaluateExpr e1 env
                                value2 <- evaluateExpr e2 env
                                pure (value1 == value2)

evaluateOper (Ex e) env = evaluateExpr e env

evaluateExpr :: Expr -> Env -> IO Bool
evaluateExpr (Paren o) env = evaluateOper o env
evaluateExpr (Not e) env = do
                            value <- evaluateExpr e env
                            pure (not value)

evaluateExpr (BoolValue b) _ = pure b

evaluateExpr (Variable v) env =
    case lookup v env of
        Just value -> pure value
        --handle like print A, if A has no value
        Nothing -> error (show v ++ " has no value")

evaluateExpr Input env = do
                          putStr "Enter value (t/f): "
                          inputval <- getLine
                          case inputval of
                            "t" -> pure True
                            "f" -> pure False
                            _ -> do
                                  evaluateExpr Input env


updateEnv :: Var -> Bool -> Env -> Env
updateEnv v value [] = [(v, value)]
updateEnv v value ((name, oldValue) : restEnv)
    | v == name = (name, value) : restEnv
    | otherwise = (name, oldValue) : updateEnv v value restEnv