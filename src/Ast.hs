module Ast where

type ClassName     = String
type FieldName     = String
type MethodName    = String
type ParameterName = String
type ObjectName    = String
type LabelName     = String

-- D (declarations)

data Class = Class { className    :: ClassName
                   , classUsage   :: Usage
                   , classFields  :: [Field]
                   , classMethods :: [Method]
                   } deriving (Show)

-- F (Fields)

data Field = Field { fieldType :: FieldType
                   , fieldName :: FieldName 
                   } deriving (Show)

-- z (FieldType)

data FieldType = ClassFieldType ClassName
               | BaseFieldType 
                 deriving (Show)

-- M (methods) 
data Method = Method { returnType :: Type
                     , methodName :: MethodName
                     , parameter1 :: Parameter
                     , parameter2 :: Parameter
                     , body       :: Expression
                     } deriving (Show)

data Parameter = Parameter { parameterFrom  :: Type
                           , parameterTo    :: Type
                           , parameterName :: String
                           } deriving (Show)

-- r (references)
data Reference = RefParameter ParameterName
               | RefField FieldName
                 deriving (Show)

-- v (values)

data Value = ValueBase BaseValue
           | ValueReference Reference
              deriving (Show)

-- b (BaseValues)
data BaseValue = BaseUnit
               | BaseNull
               | BaseBool Bool
                 deriving (Show)

-- t (Types)

data Type = ClassType ClassName Usage
          | BoolType
          | VoidType
          | BotType
            deriving (Show, Eq)

-- e (expressions)
data Expression = ExpressionSeq Expression Expression
                | ExpressionAssign FieldName Expression
                | ExpressionCall Reference MethodName Value Value
                | ExpressionValue Value
                | ExpressionNew ClassName
                | ExpressionIf Expression Expression Expression
                | ExpressionLabel LabelName Expression
                | ExpressionContinue LabelName
                | ExpressionBinaryOperation BinaryOperator Expression Expression
                | ExpressionPrint Value -- does not read value only prints it
                  deriving (Show)

data BinaryOperator = OpEQ
                    | OpAnd
                    | OpOr
                    | OpNEQ deriving (Show)

-- U
data Usage = Usage { currentUsage :: UsageImpl
                   , currentSplit :: SplitUsage
                   } 

-- s 
data SplitUsage = SplitEmpty
                | SplitRight SplitUsage
                | SplitLeft  SplitUsage
                  deriving (Eq)

-- u and w
data UsageImpl = UsageChoice UsageImpl UsageImpl
               | UsageBranch [(String, UsageImpl)]
               | UsageRecursive String UsageImpl
               | UsageVariable String
               | UsageParallel UsageImpl UsageImpl UsageImpl
               | UsageEnd
               | UsagePlaceholder
                 deriving (Eq)

instance Show Usage where
    show (Usage u s) = show u ++ "^(" ++ show s ++ ")"

instance Show SplitUsage where
    show SplitEmpty        = "e"
    show (SplitLeft rest)  = show rest ++ " * l" 
    show (SplitRight rest) = show rest ++ " * r" 

instance Show UsageImpl where
    show (UsageChoice u1 u2) = concat ["< ", show u1, ", ", show u2, " >"]
    show (UsageBranch lst)   = "{ " ++ concat [ (f ++ "; " ++ show u) 
                                              | (f, u) <- lst  ] ++ " }"
    show (UsageRecursive s u)= "rec " ++ s ++ ". " ++ show u
    show (UsageVariable s)   = s
    show (UsageParallel u1 u2 u3) = concat ["(", show u1, " | ", show u2, ").", show u3]
    show UsageEnd            = "end"
    show UsagePlaceholder    = "O"

instance Eq Usage where
    u1 == u2 = currentUsage u1 == currentUsage u2


