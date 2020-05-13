module Main where

import Mytester
import System.Environment
import Cst
import Ast
import Parser
import AstTransformer
import TypeSystem
import Data.Either (either)
import Control.Monad (mapM_, guard, when, unless)
import Data.List (permutations, nub)
import Interpreter 


runFile :: String -> IO () 
runFile s = do
    f <- readFile s
    --putStrLn f
    putStrLn "------------------------------"
    let parsed = parseProgram f
    either putStrLn (convertToAst True) parsed

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        putStrLn "no file specified should be ./program <file>"
    unless (null args) $ do
        runFile $ head args
