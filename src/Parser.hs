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

parseProgram :: String -> Either String [CstClass]
parseProgram = left show . parse parseClasses "" 

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
                                      , "end"]
            , Token.reservedOpNames = ["=", ":"]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
angles     = Token.angles     lexer -- 
brackets   = Token.brackets   lexer -- []
braces     = Token.braces     lexer -- {}
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
semi       = Token.semi       lexer -- parses a semicolon
colon      = Token.colon      lexer -- parses a colon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
dot        = Token.dot        lexer
comma      = Token.comma      lexer

parseClasses :: Parser [CstClass]
parseClasses = many (whiteSpace >> parseClass)

parseClass :: Parser CstClass 
parseClass =
    do reserved "class"
       name <- identifier
       braces $ do usage   <- parseUsage
                   fields  <- parseFields
                   methods <- parseMethods
                   return $ CstClass name usage fields methods
       

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
    where parseParameters :: Parser (CstParameter, CstParameter)
          parseParameters = do parameter1 <- parseParameter
                               comma
                               parameter2 <- parseParameter
                               return (parameter1, parameter2)
          parseParameter :: Parser CstParameter
          parseParameter = do startingType  <- parseType
                              reservedOp "->"
                              endingType    <- parseType
                              parameterName <- identifier
                              return $ CstParameter startingType endingType parameterName
       

parseType :: Parser CstType
parseType =   try parseClassType
          <|> parseType'
            where parseClassType = do c <- identifier
                                      u <- brackets $ parseUsage
                                      return $ CstClassType c u
                  parseType' = CstSimpleType <$> identifier

parseExpression :: Parser CstExpression
parseExpression =   parseSeqExpression
                <|> parseExpression'

parseSeqExpression :: Parser CstExpression
parseSeqExpression = unfoldSeqExpression <$> sepBy1 parseExpression' semi 
    where unfoldSeqExpression :: [CstExpression] -> CstExpression
          unfoldSeqExpression [x]    = x
          unfoldSeqExpression (x:xs) = CstExpressionSeq x (unfoldSeqExpression xs)

parseExpression' :: Parser CstExpression
parseExpression' =   parseNewExpression
                 <|> try parseAssignExpression
                 <|> try parseCallExpression
                 <|> parseIfExpression
                 <|> try parseLabelExpression
                 <|> parseContinueExpression
                 <|> parseBoolExpression
                 <|> parseUnitExpression
                 <|> parseNullExpression
                 <|> parseIdentifierExpression


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
       (v1, v2) <- parens $ do v1 <- identifier
                               comma
                               v2 <- identifier
                               return (v1, v2)
       return $ CstExpressionCall r m v1 v2

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

parseBoolExpression :: Parser CstExpression
parseBoolExpression =   (reserved "true"  >> return (CstExpressionBool True))
                    <|> (reserved "false" >> return (CstExpressionBool False))

parseUnitExpression :: Parser CstExpression
parseUnitExpression = reserved "unit" >> return CstExpressionUnit

parseNullExpression :: Parser CstExpression
parseNullExpression = reserved "null" >> return CstExpressionNull

parseIdentifierExpression :: Parser CstExpression
parseIdentifierExpression = CstExpressionIdentifier <$> identifier

parseUsage :: Parser CstUsage
parseUsage = parseBranchUsage

parseBranchUsage :: Parser CstUsage
parseBranchUsage =   parseEndUsage
                 <|> parseRecursiveUsage
                 <|> parseParallelUsage
                 <|> braces parseBranchUsage'
                 <|> parseVariableUsage

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
                 <|> parseBranchUsage

parseChoiceUsage' :: Parser CstUsage
parseChoiceUsage' = 
    do u1 <- parseBranchUsage
       comma
       u2 <- parseBranchUsage
       return $ CstUsageChoice u1 u2

parseParallelUsage :: Parser CstUsage
parseParallelUsage =
    do (u1, u2) <- parens parseParallelUsagePair
       dot
       u3 <- parseBranchUsage
       return $ CstUsageParallel u1 u2 u3
    

parseParallelUsagePair :: Parser (CstUsage, CstUsage)
parseParallelUsagePair = 
    do u1 <- parseBranchUsage 
       reservedOp "|"
       u2 <- parseBranchUsage
       return $ (u1, u2)


parseRecursiveUsage :: Parser CstUsage
parseRecursiveUsage = 
    do reserved "rec"
       name <- identifier
       dot
       usage <- parseBranchUsage
       return $ CstUsageRecursive name usage

parseVariableUsage :: Parser CstUsage
parseVariableUsage = 
    do name <- identifier
       return $ CstUsageVariable name

parseEndUsage :: Parser CstUsage
parseEndUsage = reserved "end" >> return CstUsageEnd


