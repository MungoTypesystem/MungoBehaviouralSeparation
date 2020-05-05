module Tester where

import Cst
import Ast
import Parser
import AstTransformer
import TypeSystem
import Data.Either (either)
import Control.Monad (mapM_, guard)
import Data.List (permutations, nub)

run :: IO () 
run = do
    f <- readFile "testfile.mg"
    --putStrLn f
    putStrLn "------------------------------"
    let parsed = parseProgram f
    either putStrLn convertToAst parsed
    
convertToAst :: [CstClass] -> IO ()
convertToAst program = do
    --mapM_ (putStrLn . show) program
    putStrLn "------------------------------"
    let converted = convert program
    either printErrors printAst converted

printErrors errors = do 
    mapM_ putStrLn errors

printAst program = do
    --mapM_ (putStrLn . show) program
    putStrLn "------------------------------"
    let typeChecked = checkTProg program
    putStrLn $ case typeChecked of
                    Left err -> show err
                    Right _  -> "program checked successfully"
    


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


