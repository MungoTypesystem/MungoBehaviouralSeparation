module Cst where

data CstClass = CstClass { cstClassName    :: String
                         , cstClassUsage   :: CstUsage
                         , cstClassFields  :: [CstField]
                         , cstClassMethods :: [CstMethod]
                         } deriving (Show)

data CstField = CstField { cstFieldType :: String
                         , cstFieldName :: String
                         } deriving (Show)

data CstMethod = CstMethod { cstMethodType   :: CstType 
                           , cstMethodName   :: String
                           , cstMethodParams :: (CstParameter, CstParameter)
                           , cstMethodExpression :: CstExpression
                           } deriving (Show)

data CstParameter = CstParameter CstType CstType String
                    deriving (Show)

data CstType = CstSimpleType String
             | CstClassType String CstUsage
               deriving (Show)

data CstExpression = CstExpressionNew String
                   | CstExpressionAssign String CstExpression
                   | CstExpressionCall String String String String
                   | CstExpressionSeq CstExpression CstExpression
                   | CstExpressionIf CstExpression CstExpression CstExpression
                   | CstExpressionLabel String CstExpression
                   | CstExpressionContinue String
                   | CstExpressionBool Bool
                   | CstExpressionUnit
                   | CstExpressionNull
                   | CstExpressionIdentifier String
                   | CstExpressionPrint String
                     deriving (Show)
        

data CstUsage = CstUsageChoice CstUsage CstUsage
              | CstUsageBranch [(String, CstUsage)]
              | CstUsageParallel CstUsage CstUsage CstUsage
              | CstUsageRecursive String CstUsage
              | CstUsageVariable String
              | CstUsageEnd
                deriving (Show)
