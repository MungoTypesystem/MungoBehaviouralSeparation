module Interpreter where

import Ast
import Control.Monad.State
import ListUtils

data Loc = Loc Int String
           deriving (Eq, Show)

data Values = LocValue Loc
            | BValue BaseValue
            deriving (Show) -- todo better

data EnvP   = EnvP String Values String Values
data EnvF   = EnvF [(String, Values)]
data Heap   = Heap [(Loc, EnvF)]

data ST = ST { envP    :: EnvP
             , heap    :: Heap
             , active  :: Loc
             , nextLoc :: Int
             , classes :: [Class]
             }

type Runtime a = StateT ST IO a


runProgram :: [Class] -> Class -> Method -> IO ()
runProgram allClasses cls m = 
    do let x1    = parameterName $ parameter1 m
       let x2    = parameterName $ parameter2 m
       let envP' = EnvP x1 (BValue BaseUnit) x2 (BValue BaseUnit)
       let envF  = EnvF $ map initField (classFields cls)
       let o     = Loc 0 (className cls) 
       let h     = Heap $ [(o, envF)]
       let next  = 1
       let st = ST { envP = envP'
                   , heap = h
                   , active = o
                   , nextLoc = next
                   , classes = allClasses } 
       runStateT (runExpression (body m)) st
       putStrLn "done"

io :: IO a -> Runtime a
io = liftIO

getF :: (ST -> a) -> Runtime a
getF f = f <$> get

getClass :: String -> Runtime Class
getClass cn =
    do classes <- getF classes
       return $ findValueP ((cn ==). className) classes

getMethod :: String -> String -> Runtime Method
getMethod cn m = 
    do cls <- getClass cn
       return $ findValueP ((m ==). methodName) $ classMethods cls

getEnvP :: Runtime EnvP
getEnvP = getF envP

putEnvP :: EnvP -> Runtime ()
putEnvP envP' =
    do s <- get 
       put $ s { envP = envP' }

getHeap :: Runtime Heap
getHeap = getF heap

putActive :: Loc -> Runtime ()
putActive l =
    do s <- get 
       put $ s{active =  l}

getActive :: Runtime Loc
getActive = getF active 

getEnvF :: Runtime EnvF
getEnvF =
    do o        <- getActive
       (Heap h) <- getHeap
       return $ findValue o h -- snd . head $ filter ((== o) . fst) h

putEnvF :: EnvF -> Runtime ()
putEnvF envF =
    do s        <- get
       o        <- getActive
       (Heap h) <- getHeap
       let h' = Heap $ updateKey o envF h
       put $ s { heap = h' }
       return ()

writeField :: FieldName -> Values -> Runtime ()
writeField f v =
    do o <- getActive 
       (EnvF envF) <- getEnvF
       let envF' = EnvF $ updateKey f v envF 
       putEnvF envF'

putHeap :: Heap -> Runtime ()
putHeap h = 
    do s <- get
       put $ s {heap = h}

runReference :: Reference -> Runtime Values
runReference (RefParameter pname) = 
    do (EnvP x1 v1 x2 v2) <- getEnvP
       return $ if x1 == pname then v1 else v2
runReference (RefField fname)     = 
    do (EnvF lst) <- getEnvF
       return $ findValue fname lst

newLoc :: String -> Runtime Loc
newLoc cn = 
    do locID <- getF nextLoc
       let loc = Loc locID cn
       s <- get
       put $ s { nextLoc = locID + 1}
       return loc

runExpression :: Expression -> Runtime Values
runExpression (ExpressionSeq e1 e2)      = runExpression e1 >> runExpression e2
runExpression (ExpressionAssign f e)     = runExpressionAssign f e
runExpression (ExpressionCall r m v1 v2) = runExpressionCall r m v1 v2
runExpression (ExpressionValue val)      = runExpressionValue val
runExpression (ExpressionNew cn)         = runExpressionNew cn
runExpression (ExpressionIf e1 e2 e3)    = runExpressionIf e1 e2 e3
runExpression (ExpressionLabel lbl e)    = runExpressionLabel lbl e
runExpression (ExpressionContinue _)     = runExpressionContinue
runExpression (ExpressionPrint v)        = runExpressionPrint v

runExpressionPrint :: Value -> Runtime Values
runExpressionPrint v =
    do v' <- runExpressionValue v 
       io . putStrLn $ show v'
       return $ BValue BaseUnit

runExpressionCall :: Reference -> String -> Value -> Value -> Runtime Values
runExpressionCall r m v1 v2 = 
    do v1' <- runExpressionValue v1
       v2' <- runExpressionValue v2
       (LocValue o'@(Loc _ cn)) <- runReference r
       mthd <- getMethod cn m
       let x1 = parameterName $ parameter1 mthd
       let x2 = parameterName $ parameter2 mthd
       oldEnvP <- getEnvP
       let envP = EnvP x1 v1' x2 v2'
       putEnvP envP
       o <- getActive
       putActive o'
       v <- runExpression $ body mthd
       (EnvP _ v1'' _ v2'') <- getEnvP
       -- references so no problem
       putEnvP oldEnvP
       putActive o
       return v

runExpressionContinue :: Runtime Values
runExpressionContinue =
    do io $ putStrLn "continue expression met at runtime this should not happen"
       return $ BValue BaseUnit
      

substitute :: String -> Expression -> Expression -> Expression
substitute lbl e' (ExpressionSeq e1 e2)       = ExpressionSeq e1 $ substitute lbl e' e2
substitute lbl e' (ExpressionIf e1 e2 e3)     = ExpressionIf e1 (substitute lbl e' e2) (substitute lbl e' e3)
substitute lbl e' e@(ExpressionContinue lbl') = if lbl' == lbl then e' else e
substitute lbl _  e@(ExpressionAssign _ _)    = e

runExpressionLabel :: String -> Expression -> Runtime Values
runExpressionLabel lbl e = 
    do runExpression $ substitute lbl e e

runExpressionIf :: Expression -> Expression -> Expression -> Runtime Values
runExpressionIf e1 e2 e3 = 
    do (BValue (BaseBool cond)) <- runExpression e1
       if cond
            then runExpression e2
            else runExpression e3

initVal :: FieldType -> Values
initVal (ClassFieldType cn) = BValue BaseNull
initVal (BaseFieldType) = BValue $ BaseBool False

initField :: Field -> (String, Values)
initField field = 
    let val  = initVal $ fieldType field
        name = fieldName field  
    in (name, val)

runExpressionNew :: String -> Runtime Values
runExpressionNew cn = 
    do cls <- getClass cn
       o <- newLoc cn
       (Heap h) <- getHeap
       let envFValues = map initField $ classFields cls
       let envF = EnvF envFValues
       let h' = Heap $ (o, envF) : h
       putHeap h'
       return $ LocValue o

runExpressionValue :: Value -> Runtime Values
runExpressionValue (ValueBase bv)       = return $ BValue bv
runExpressionValue (ValueReference ref) = runReference ref

runExpressionAssign :: FieldName -> Expression -> Runtime Values
runExpressionAssign f e = 
    do v <- runExpression e
       writeField f v
       return $ BValue (BaseUnit) 

