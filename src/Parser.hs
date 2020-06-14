module Parser where

import Cst
import System.IO
import Control.Monad
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Arrow (left)
import Data.Maybe
import Text.ParserCombinators.Parsec.Number
import Data.Either (partitionEithers)

parseProgram :: String -> Either String [CstClass]
parseProgram = left show . parse (parseClasses <* eof) "" 

-- test parser
testParseUsage = parse parseUsage ""
testParseExpression = parse parseExpression ""
testParseType = parse parseType ""
testParseMethod = parse parseMethod ""
testParseClass = parse parseClass ""


languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "continue"
                                      , "true"
                                      , "false"
                                      , "new"
                                      , "rec"
                                      , "class"
                                      , "end"
                                      , "print"
                                      , "input"
                                      , "unit"
                                      , "null" ]
            , Token.reservedOpNames = ["=", ":", "==", "!=", "&&", "||", "+", "-", "<", ">", "<=", ">="]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier    lexer -- parses an identifier
reserved   = Token.reserved      lexer -- parses a reserved name
reservedOp = Token.reservedOp    lexer -- parses an operator
angles     = Token.angles        lexer -- 
brackets   = Token.brackets      lexer -- []
braces     = Token.braces        lexer -- {}
parens     = Token.parens        lexer -- parses surrounding parenthesis:
                                       --   parens p
                                       -- takes care of the parenthesis and
                                       -- uses p to parse what's inside them
semi       = Token.semi          lexer -- parses a semicolon
colon      = Token.colon         lexer -- parses a colon
whiteSpace = Token.whiteSpace    lexer -- parses whitespace
dot        = Token.dot           lexer
comma      = Token.comma         lexer
strParser  = Token.stringLiteral lexer

parseClasses :: Parser [CstClass]
parseClasses = many (whiteSpace >> parseClass)

parseClass :: Parser CstClass 
parseClass =
    do reserved "class"
       name <- identifier
       usage <- brackets parseUsage
       braces $ do 
                   --fields  <- parseFields
                   --methods <- parseMethods
                   fieldsAndClasses <- parseMethodsAndFields
                   let (fields, methods) = partitionEithers fieldsAndClasses
                   return $ CstClass name usage fields methods
       
parseMethodsAndFields :: Parser [Either CstField CstMethod]
parseMethodsAndFields = 
    many1 parseMethodOrField  

parseMethodOrField :: Parser (Either CstField CstMethod)
parseMethodOrField = 
    (Right <$> try parseMethod) <|> (Left <$> parseField)

parseFields :: Parser [CstField]
parseFields = parseField `manyTill` lookAhead (try parseMethod)

parseField :: Parser CstField
parseField = liftM2 CstField identifier identifier 

parseMethods :: Parser [CstMethod]
parseMethods = many1 parseMethod

parseMethod :: Parser CstMethod
parseMethod = 
    do returnType <- parseType
       name       <- identifier
       params     <- parens parseParameters
       body       <- braces parseExpression
                        
       return $ CstMethod returnType name params body
parseParameters :: Parser (CstParameter, CstParameter)
parseParameters =   try parseFullParameters
                <|> try parseHalfParameter
                <|> parseEmptyParameter
    where parseFullParameters = do parameter1 <- parseParameter
                                   comma
                                   parameter2 <- parseParameter 
                                   return (parameter1, parameter2)
          parseHalfParameter = do parameter1 <- parseParameter
                                  parameter2 <- voidParameter 2
                                  return (parameter1, parameter2)
          parseEmptyParameter = do parameter1 <- voidParameter 1 
                                   parameter2 <- voidParameter 2
                                   return (parameter1, parameter2)


          voidParameter n = return $ CstParameter (CstSimpleType "void") (CstSimpleType "void") ("$param"++show n)

parseParameter :: Parser CstParameter
parseParameter =   try parseFullParameter 
               <|> try parseHalfParameter
    where parseFullParameter = do startingType  <- parseType
                                  reservedOp "->"
                                  endingType    <- try parseType
                                  parameterName <- identifier
                                  return $ CstParameter startingType endingType parameterName
          parseHalfParameter = do startingType  <- parseType
                                  parameterName <- identifier
                                  return $ CstParameter startingType startingType parameterName
       

parseType :: Parser CstType
parseType =   try parseClassType
          <|> parseType'
            where parseClassType = do c <- identifier
                                      u <- brackets $ parseUsage
                                      return $ CstClassType c u
                  parseType' = CstSimpleType <$> identifier

parseExpression :: Parser CstExpression
parseExpression =   parseSeqExpression
                <|> parseExpression''

parseExpression'' = try parseBinaryExpression
                  <|> parseExpression'

parseSeqExpression :: Parser CstExpression
parseSeqExpression = unfoldSeqExpression <$> sepBy1 parseExpression'' semi 
    where unfoldSeqExpression :: [CstExpression] -> CstExpression
          unfoldSeqExpression [x]    = x
          unfoldSeqExpression (x:xs) = CstExpressionSeq x (unfoldSeqExpression xs)

parseBinaryExpression :: Parser CstExpression
parseBinaryExpression = do 
   e1 <- parseExpression'
   whiteSpace
   op <- parseBinaryOperator
   e2 <- parseExpression''
   return $ CstBinaryExpression op e1 e2


parseBinaryOperator :: Parser CstBinaryOperator
parseBinaryOperator = 
       (reservedOp "==" >> return CstOpEQ)
   <|> (reservedOp "!=" >> return CstOpNEQ)
   <|> (reservedOp "&&" >> return CstOpAnd)
   <|> (reservedOp "||" >> return CstOpOr)
   <|> (reservedOp "+" >> return CstOpAdd)
   <|> (reservedOp "-" >> return CstOpSub)
   <|> (reservedOp "<" >> return CstOpLT)
   <|> (reservedOp ">" >> return CstOpGT)
   <|> (reservedOp "<=" >> return CstOpLEQ)
   <|> (reservedOp ">=" >> return CstOpGEQ)

parseExpression' :: Parser CstExpression
parseExpression' =   parens parseExpression
                 <|> parseNewExpression
                 <|> try parseAssignExpression
                 <|> try parseCallExpression
                 <|> parseIfExpression
                 <|> try parseLabelExpression
                 <|> parseContinueExpression
                 <|> try parsePrintExpression
                 <|> try parseInputExpression
                 <|> parseValueExpression

parseValueExpression :: Parser CstExpression
parseValueExpression = CstExpressionValue <$> parseValue

parseValue :: Parser CstValue
parseValue =   (reserved "true" >> return (CstBool True))
                 <|> (reserved "false" >> return(CstBool False))
                 <|> (reserved "null" >> return CstNull)
                 <|> (reserved "unit" >> return CstUnit)
                 <|> (CstString <$> strParser)
                 <|> (CstInteger <$> int)
                 <|> (CstReference <$> identifier)
                 
parsePrintExpression :: Parser CstExpression
parsePrintExpression =
    do reserved "print"
       v <- parens parseValue
       return $ CstExpressionPrint v

parseInputExpression :: Parser CstExpression
parseInputExpression =
    do reserved "input"
       parens whiteSpace
       return $ CstExpressionInput 

parseNewExpression :: Parser CstExpression
parseNewExpression =
    do reserved "new"
       name <- identifier
       return $ CstExpressionNew name

parseAssignExpression:: Parser CstExpression
parseAssignExpression = 
    do r <- identifier
       reservedOp "="
       e <- parseExpression' 
       return $ CstExpressionAssign r e

parseCallExpression :: Parser CstExpression
parseCallExpression =
    do r <- identifier
       dot
       m <- identifier
       (v1, v2) <- parens $ try fullParam <|> try halfParam <|> noParam
       return $ CstExpressionCall r m v1 v2
    where fullParam = do v1 <- parseValue
                         comma
                         v2 <- parseValue
                         return (v1, v2)
          halfParam = do v1 <- parseValue
                         return (v1, CstUnit)
          noParam = return (CstUnit, CstUnit)
          

parseIfExpression :: Parser CstExpression
parseIfExpression = 
    do reserved "if"
       e1 <- parens parseCallExpression
       e2 <- braces parseExpression
       reserved "else"
       e3 <- braces parseExpression
       return $ CstExpressionIf e1 e2 e3

parseLabelExpression :: Parser CstExpression
parseLabelExpression = 
    do label <- identifier
       reservedOp ":"
       e <- parseExpression
       return $ CstExpressionLabel label e

parseContinueExpression :: Parser CstExpression
parseContinueExpression =
    do reserved "continue"
       label <- identifier
       return $ CstExpressionContinue label


parseUsage :: Parser CstUsage
parseUsage = 
    do lst <- sepBy1 parseBranchUsage semi
       return $ unfoldSeqUsage lst
        where unfoldSeqUsage [x]    = x
              unfoldSeqUsage (x:xs) = CstUsageParallel x CstUsageEnd (unfoldSeqUsage xs)

    --   try parseSeqUsage
            -- <|> parseBranchUsage

parseBranchUsage :: Parser CstUsage
parseBranchUsage =   parseEndUsage
                 <|> parseRecursiveUsage
                 <|> parseParallelUsage
                 <|> braces parseBranchUsage'
                 <|> parseVariableUsage

parseSeqUsage :: Parser CstUsage
parseSeqUsage = 
    do u1 <- parseBranchUsage
       semi
       u2 <- parseBranchUsage
       return $ CstUsageParallel u1 CstUsageEnd u2

parseBranchUsage' :: Parser CstUsage
parseBranchUsage' = 
    do list <- many1 parseBranchUsagePair
       return $ CstUsageBranch list
        where parseBranchUsagePair :: Parser (String, CstUsage) 
              parseBranchUsagePair = 
                    do name <- identifier
                       semi
                       usage <- parseChoiceUsage
                       return $ (name, usage)
    
parseChoiceUsage :: Parser CstUsage
parseChoiceUsage =   angles parseChoiceUsage'
                 <|> parseUsage

parseChoiceUsage' :: Parser CstUsage
parseChoiceUsage' = 
    do u1 <- parseUsage
       comma
       u2 <- parseUsage
       return $ CstUsageChoice u1 u2

parseParallelUsage :: Parser CstUsage
parseParallelUsage = 
    do parallel <- parens parseParallelUsagePair'
       dot
       u3 <- parseUsage
       return $ combineUsage u3 parallel
        where combineUsage :: CstUsage -> [CstUsage] -> CstUsage
              combineUsage u3 (x:y:[]) = CstUsageParallel x y u3 
              combineUsage u3 (x:[])   = CstUsageParallel x CstUsageEnd u3
              combineUsage u3 []       = u3
              combineUsage u3 (x:xs)   = CstUsageParallel x (combineUsage u3 xs) CstUsageEnd

parseParallelUsagePair' :: Parser [CstUsage]
parseParallelUsagePair' =  sepBy1 parseUsage (reservedOp "|")

parseRecursiveUsage :: Parser CstUsage
parseRecursiveUsage = 
    do reserved "rec"
       name <- identifier
       dot
       usage <- parseUsage
       return $ CstUsageRecursive name usage

parseVariableUsage :: Parser CstUsage
parseVariableUsage = 
    do name <- identifier
       return $ CstUsageVariable name

parseEndUsage :: Parser CstUsage
parseEndUsage = reserved "end" >> return CstUsageEnd


