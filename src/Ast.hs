module Ast where

import Data.List (sortBy)

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
               | BaseFieldType  SimpleType
                 deriving (Show)

data SimpleType = BoolType 
              | StringType
              | IntegerType
              | VoidType
              deriving (Show, Eq)

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
               | BaseString String
               | BaseInteger Integer
               | BaseBool Bool
                 deriving (Show, Eq)

-- t (Types)

data Type = ClassType ClassName Usage
          | BaseType SimpleType
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
                | ExpressionInput 

data BinaryOperator = OpEQ
                    | OpAnd
                    | OpOr
                    | OpNEQ
                    | OpAdd
                    | OpSub deriving (Show, Eq)

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

instance Eq UsageImpl where
    (UsageChoice u1 u2)      == (UsageChoice u1' u2')       = u1 == u1' && u2 == u2'
    (UsageBranch lst)        == (UsageBranch lst')          = lst == lst'
    (UsageRecursive l u)     == (UsageRecursive l' u')      = l == l' && u == u'
    (UsageRecursive l u)     == (UsageVariable l')          = l == l' 
    (UsageVariable l)        == (UsageRecursive l' u')      = l == l' 
    (UsageVariable l)        == (UsageVariable l')          = l == l' 
    (UsageParallel u1 u2 u3) == (UsageParallel u1' u2' u3') = u1 == u1' && u2 == u2' && u3 == u3'
    (UsageEnd)               == (UsageEnd)                  = True
    (UsagePlaceholder)       == (UsagePlaceholder)          = True
    _                        == _                           = False

instance Show Usage where
    show (Usage u s) = show u ++ "^(" ++ show s ++ ")"

instance Show SplitUsage where
    show SplitEmpty        = "e"
    show (SplitLeft rest)  = show rest ++ " * l" 
    show (SplitRight rest) = show rest ++ " * r" 

instance Show UsageImpl where
    show (UsageChoice u1 u2) = concat ["< ", show u1, ", ", show u2, " >"]
    show (UsageBranch lst)   = "{ " ++ concat [ (f ++ "; " ++ show u ++ " ") 
                                              | (f, u) <- lst  ] ++ " }"
    show (UsageRecursive s u)= "rec " ++ s ++ ". " ++ show u
    show (UsageVariable s)   = s
    show (UsageParallel u1 u2 u3) = concat ["(", show u1, " | ", show u2, ").", show u3]
    show UsageEnd            = "end"
    show UsagePlaceholder    = "O"

instance Eq Usage where
    u1 == u2 = currentUsage u1 == currentUsage u2

instance Show Expression where
    show (ExpressionSeq e1 e2)      = show e1 ++ "; " ++ show e2
    show (ExpressionAssign f e)     = f ++ " = " ++ show e
    show (ExpressionCall r m v1 v2) = concat [show r, ".", m, "(", show v1, ", ", show v2, ")"]
    show (ExpressionValue v)        = show v
    show (ExpressionNew cn)         = "new " ++ cn
    show (ExpressionIf e1 e2 e3)    = concat ["if (", show e1, ") { ", show e2, " } else { " , show e3, " } "]
    show (ExpressionLabel lbl e)    = lbl ++ ": " ++ show e
    show (ExpressionContinue lbl)   = "continue " ++ lbl
    show (ExpressionBinaryOperation b e e') =
        concat ["((", show e, ") ", show b, " (", show e', "))"]
    show (ExpressionPrint v)        = "print(" ++ show v ++ ")"
    show (ExpressionInput)          = "input()"



