module Minsky where

import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Number
import Data.Either
import Data.List

languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "INC"
                                      , "JZDEC" ]
            , Token.reservedOpNames = []
            }

lexer = Token.makeTokenParser languageDef

reserved   = Token.reserved      lexer -- parses a reserved name
colon      = Token.colon         lexer -- parses a colon
whiteSpace = Token.whiteSpace    lexer -- parses whitespace

data MinskyOperation = INC Integer Integer | JZDEC Integer Integer Integer | HALT deriving Show
data MinskyInstruction = Instruction Integer MinskyOperation deriving Show

parseString = parse parseProgram ""

parseProgram :: Parser [MinskyInstruction]
parseProgram = many (whiteSpace >> parseInstruction)

parseInstruction :: Parser MinskyInstruction
parseInstruction = do
    label <- int
    colon
    whiteSpace
    operation <- parseOperation
    return $ Instruction label operation

parseOperation :: Parser MinskyOperation
parseOperation =        
        (reserved "INC" >> do
            whiteSpace
            reg <- int
            whiteSpace
            next <- int
            return (INC reg next))
    <|> (reserved "JZDEC" >> do
            whiteSpace
            reg <- int
            whiteSpace
            true <- int
            whiteSpace
            false <- int
            return (JZDEC reg true false)) 
    <|> (reserved "HALT" >> do return HALT) 

boolValidatorClass = "class BoolValidator[{validate;<end, end>}] { bool validate(bool b) { b } }"
mainClass start = "class main[{main; end}] {Inst machine void main() { machine = new Inst; machine.i" ++ (show start) ++ "(0, 0) }}"

instClass :: [Integer] -> [String] -> String
instClass labels methods = "class Inst[{" ++ (generateUsage labels) ++ "}] { Inst i bool b BoolValidator v int r1 int r2 \n" ++ (intercalate "\n" methods) ++ "}"
generateUsage :: [Integer] -> String
generateUsage labels = 
    let parts = map (\label-> "i" ++ (show label) ++ "; end ") labels
    in intercalate " " parts


opToMethod :: MinskyOperation -> String
opToMethod HALT = "print(r1);print(r2)"
opToMethod (INC reg next) = "i = new Inst; r" ++ (show reg) ++ " = (r" ++ (show reg) ++ " + 1);" ++ (call next)
opToMethod (JZDEC reg true false) = "i = new Inst; b = (r" ++ (show reg) ++ " == 0); v = new BoolValidator; if (v.validate(b)) {" ++ call true ++ "} else { r" ++ (show reg) ++ " = (r" ++ (show reg) ++ " - 1); " ++ call false ++" }; v = null"

createMethod :: MinskyInstruction -> String
createMethod (Instruction label inst) = "void i" ++ (show label) ++ "(int x1, int x2) { r1 = x1; r2 = x2; " ++ (opToMethod inst) ++ "; i = null}"

call :: Integer -> String
call label = "i.i" ++ (show label) ++ "(r1, r2)"

runFile :: String -> IO()
runFile s = do
    f <- readFile s
    let progParsed = fromRight [] $ parseString f 
    let res = generateClasses progParsed 
    putStrLn res
    return ()


getLabel (Instruction label _) = label

cmp (Instruction l1 _) (Instruction l2 _) = compare l1 l2

generateClasses :: [MinskyInstruction] -> String
generateClasses inst = 
    let sorted = sortBy cmp inst
        main = mainClass $ getLabel (head sorted)
        instMethods = map createMethod sorted
        labels = map (\(Instruction label inst) -> label) sorted
        machine = instClass labels instMethods
    in
        main ++ "\n" ++ boolValidatorClass ++ "\n" ++ machine