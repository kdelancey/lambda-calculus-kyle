module STEvaluator where
    import STParser
    import Data.List
    import Text.Regex.Applicative

    evalST :: Lambda -> [Maybe a] -> Maybe a
    evalST (Lambda ctx exprs) inList = 
        if (length $ inList) == (length $ ctx) 
            then evalLambda (zip ctx inList) exprs
            else error "The length of the input list did not match the number of terms needed."

    evalLambda :: [(Variable,Maybe a)] -> [Expr] -> Maybe a
    evalLambda ctx []       = Nothing
    evalLambda ctx (ex:exs) = 
        case ex of 
            (Var  var)     -> 
                let varName = (getName var) in
                    if (typeCheck ctx var Number) 
                        then getCtxMaybe ctx varName --currently will only iterate once
                        else error $ typeErrMsg varName Number
                        -- need case for Bol operations
            (Add  ex1 ex2) -> (evalIntOpr (ctx) (+) (ex1) (ex2))
            (Sub  ex1 ex2) -> (evalIntOpr (ctx) (-) (ex1) (ex2))
            (Div  ex1 ex2) -> (evalIntOpr (ctx) (/) (ex1) (ex2))
            (Mult ex1 ex2) -> (evalIntOpr (ctx) (*) (ex1) (ex2))
            -- (Add  ex1 ex2) -> (ex, (evalIntOpr ctx (+) ex1 ex2)):(evalLambda ctx exs)
            -- (Sub  ex1 ex2) -> (ex, (evalIntOpr ctx (-) ex1 ex2)):(evalLambda ctx exs)
            -- (Div  ex1 ex2) -> (ex, (evalIntOpr ctx (/) ex1 ex2)):(evalLambda ctx exs)
            -- (Mult ex1 ex2) -> (ex, (evalIntOpr ctx (*) ex1 ex2)):(evalLambda ctx exs)

    evalIntOpr :: [(Variable, Maybe a)] -> (a -> a -> a) -> Expr -> Expr -> Maybe a
    evalIntOpr ctx opr pExp1 pExp2 = let eval1 = evalLambda ctx [pExp1] in
                                        let eval2 = evalLambda ctx [pExp2] in
                                            doIntOpr eval1 eval2 opr
                                            
    doIntOpr :: Maybe a -> Maybe a -> (a -> a -> a) -> Maybe a
    doIntOpr (Just x) (Just y) opr = Just $ opr x y
    doIntOpr _ _ _ = error $ "Bad Integer Operation"

    typeCheck :: [(Variable, Maybe a)] -> Variable -> Type -> Bool
    typeCheck ctx var expTy = let varName = getName var in
                                if ((expectedType ctx varName) == expTy)
                                    then True
                                    else False

    typeErrMsg :: Name -> Type -> String
    typeErrMsg nm ty = nm ++ " has incorrect type. Expected " ++ tyStr
                        where
                            tyStr = case ty of
                                Number -> "Number"
                                Float -> "Float"
                                Str -> "Str"
                                Bol -> "Bol"
                                X -> "Unassigned"

    expectedType :: [(Variable, Maybe a)] -> Name -> Type
    expectedType [] cmp = error $ cmp ++ " not in context."
    expectedType ((Variable nm ty, val):vs) cmp = if (nm == cmp) 
                                                    then ty
                                                    else expectedType vs cmp

    changeMaybe :: [(Variable, Maybe a)] -> Name -> Maybe a -> [(Variable, Maybe a)]
    changeMaybe [] cmp = []
    changeMaybe ((Variable nm ty, cVal):vs) cmp nVal = if (nm == cmp) 
                                                        then (Variable nm ty, nVal):vs
                                                        else (Variable nm ty, cVal):(changeMaybe vs cmp nVal)
    
    getCtxMaybe :: [(Variable, Maybe a)] -> Name -> Maybe a
    getCtxMaybe [] cmp = []
    getCtxMaybe ((Variable nm ty, cVal):vs) cmp = if (nm == cmp) 
                                                    then cVal
                                                    else getCtxMaybe vs cmp


    main = do 
        input <- [Maybe 3, Maybe 4, Maybe 10] -- [Maybe a] List
        print "Testing different Steps of Parser:"
        print "Name:"
        print $ "nameOfAVariable" =~ name
        print "Type:"
        print $ ":Int" =~ type'
        print "Variable Declarations:"
        print $ "nameOfAVariable:Int" =~ variableDec
        print "Variable Declarations:"
        print $ "& a   :   Int   b:Int -> " =~ variableDecs
        print "Variable as Expr:"
        print $ "a" =~ variable
        print "Expression:"
        print $ "a * b" =~ expr
        print "Single Lambda:"
        print $ "& a   :   Int   b:Int -> & a   :   Int   b:Int -> a * b" =~ lambda
        print "Done Tests"
    
    -- typeEq :: Variable -> Variable -> Bool
    -- typeEq (Variable aN aT) (Variable bN bT) = (aT == bT)

    -- nameEq :: Variable -> Variable -> Bool
    -- nameEq (Variable aN aT) (Variable bN bT) = (aN == bN)