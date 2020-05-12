module Mytester where

import Cst
import Ast
import Parser
import AstTransformer
import TypeSystem
import Data.Either (either)
import Control.Monad (mapM_, guard, when)
import Data.List (permutations, nub)
import Interpreter 

houseF = "../ExamplePrograms/house.mg"

run :: String -> Bool -> IO () 
run name toRun = do
    f <- readFile name
    --putStrLn f
    putStrLn "------------------------------"
    let parsed = parseProgram f
    either putStrLn (convertToAst toRun) parsed
    
convertToAst :: Bool -> [CstClass] -> IO ()
convertToAst toRun program = do
    --mapM_ (putStrLn . show) program
    putStrLn "------------------------------"
    let converted = convert program
    either printErrors (printAst toRun) converted

printErrors errors = do 
    mapM_ putStrLn errors

printAst toRun program = do
    --mapM_ (putStrLn . show) program
    putStrLn "------------------------------"
    let typeChecked = checkTProg program
    case typeChecked of
            Left err -> putStrLn $ show err
            Right _  -> do putStrLn "program checked successfully" 
                           when (toRun) $ runMain program

runMain program = do
    putStrLn "------------------------------"
    -- find the class with the main function
    let cls = filter ((== "main") . className) program

    when (length cls /= 1) $ do
        putStrLn "could not find main class"

    when (length cls == 1) $ do
        let cls' = head cls
        let mthd = filter ((== "main") . methodName) (classMethods cls')
        when (length mthd /= 1) $ do
            putStrLn "could not find main method"
        
        when (length mthd == 1) $ do
            let mthd' = head mthd
            let tp1  = parameterFrom $ parameter1 $ mthd'
            let tp2  = parameterFrom $ parameter2 $ mthd'
            let tret = returnType mthd' 
            let void = BaseType VoidType
            let allVoid = (tp1 == void && tp2 == void && tret == void)
            when allVoid $ do
                runProgram program cls' mthd'
            when (not allVoid) $ do
                putStrLn $ "parameters and return type must be of type void for main.main()"
    

(Right u1) = testParseUsage "rec X. {m; <end, X>}"
(Right u1') = convertUsage u1 []
u1'' = (Usage u1' SplitEmpty)

(Right u2) = testParseUsage "( {a; end} | {b; end}).{f ; end} "
(Right u2') = convertUsage u2 []
u2'' = (Usage u2' SplitEmpty)

(Right u3) = testParseUsage "( {a; end} | ({b; end} | {c;end}).{e;end} ).{f ; end} "
(Right u3') = convertUsage u3 []
u3'' = (Usage u3' SplitEmpty)

(Right u4) = testParseUsage "(({a; end} | {b ; end}).{c ; end} | ({d; end} | {e;end}).{f;end}).{g ; end} "
(Right u4') = convertUsage u4 []
u4'' = (Usage u4' SplitEmpty)

c1 = ClassType "c1" u1'' 
c2 = ClassType "c2" u2''
c3 = ClassType "c3" u3''
c4 = ClassType "c4" u4''

f1 = ("f", c1)
f2 = ("g", c2)
f3 = ("h", c3)
f4 = ("h", c4)

s1 = splitField f1
s2 = splitField f2
s3 = splitField f3
s4 = splitField f4


