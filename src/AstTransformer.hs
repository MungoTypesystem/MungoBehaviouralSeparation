module AstTransformer where

import Cst
import Ast

import Control.Monad (liftM2, when, liftM)
import Data.Either (partitionEithers)


type Error a = Either [String] a

combineErrors :: Error a -> Error b -> (a -> b -> c) -> Error c
combineErrors (Left err1) (Left err2) f = Left (err1 ++ err2)
combineErrors l           r           f = liftM2 f l r  

data Definitions = Definitions { globalClassNames  :: [String]
                               , currentClassName  :: String
                               , currentLabels     :: [String]
                               , currentParameters :: [String]
                               , currentFields     :: [String]
                               } 

classNameExists :: Definitions -> String -> Bool
classNameExists g name = name `elem` globalClassNames g

fieldNameExists :: Definitions -> String -> Bool
fieldNameExists g fname = fname `elem` currentFields g

addField :: Definitions -> String -> Definitions
addField g fname = g { currentFields = fname : (currentFields g)}

parameterExists :: Definitions -> String -> Bool
parameterExists g pname = pname `elem` currentParameters g

addParameter :: Definitions -> String -> Definitions
addParameter g pname = g { currentParameters = pname : (currentParameters g) }

containsLabel :: Definitions -> String -> Bool
containsLabel g lbl = lbl `elem` currentLabels g 

insertLabel :: Definitions -> String -> Definitions
insertLabel g lbl = g { currentLabels = lbl:(currentLabels g) }

convert :: [CstClass] -> Error [Class]
convert cstClasses = do
    -- check that names are unique 
    if null errors 
        then Right $ classes
        else Left $ concat errors
    where names  = map cstClassName cstClasses
          g      = Definitions { globalClassNames = names
                               , currentClassName = "" 
                               , currentLabels = []
                               , currentParameters = []
                               , currentFields = []
                               } 
          result = map (convertClass g) cstClasses
          (errors, classes) = partitionEithers result

convertClass :: Definitions -> CstClass -> Error Class
convertClass g cstClass = 
    do let name   = cstClassName cstClass
           g'     = g { currentClassName = name } 

       u             <- convertFullUsage (cstClassUsage cstClass)
       (g'', fields) <- convertFields g' (cstClassFields cstClass)

       let methods            = map (convertMethod g'') (cstClassMethods cstClass)
       let (errors, methods') = partitionEithers methods

       -- check that the usage can reach end
       let methodNames = map methodName methods' 
       let errors'     = concat errors ++ checkUsage u methodNames

       if null errors'
            then Right $ Class name u fields methods'
            else Left $ errors'
        

convertFields :: Definitions -> [CstField] -> Error (Definitions, [Field])
convertFields g []     = Right (g, [])
convertFields g (f:fs) = 
    do (g', fs') <- convertFields g fs
       (g'', f') <- convertField g' f
       return (g'', f':fs')

convertField :: Definitions -> CstField -> Error (Definitions, Field)
convertField g f = 
    let name  = cstFieldName f
        g'    = g `addField` name
        ftype = convertFieldType g (cstFieldType f)
    in if not (g `fieldNameExists` name)
            then (\ftype' -> (g', Field ftype' name)) <$> ftype
            else Left $ ["Field name " ++ name ++ " defined more than once"]

convertMethod :: Definitions -> CstMethod -> Error Method
convertMethod g m = 
    do let name    = cstMethodName m

       let (cstParameter1, cstParameter2) = cstMethodParams m
       (g', parameter1)  <- convertParameter g cstParameter1
       (g'', parameter2) <- convertParameter g' cstParameter2
       retType           <- convertType g'' (cstMethodType m)
       e                 <- convertExpression g'' (cstMethodExpression m)

       Right $ Method retType name parameter1 parameter2 e

convertParameter :: Definitions -> CstParameter -> Error (Definitions, Parameter)
convertParameter g (CstParameter t1 t2 name) = 
    let t1' = convertType g t1 
        t2' = convertType g t2
        g'  = g `addParameter` name
    in if g `parameterExists` name
            then Left $ ["Parameter name " ++ name ++ " already exists"]
            else if g `fieldNameExists` name
                    then Left $ ["Parameter name " ++ name ++ " already exists as a field"]
                    else combineErrors t1' t2' $ \t1'' t2'' -> (g', Parameter t1'' t2'' name)

convertFieldType :: Definitions -> String -> Error FieldType
convertFieldType g "bool" = Right (BaseFieldType BoolType)
convertFieldType g "string" = Right (BaseFieldType StringType)
convertFieldType g "int" = Right (BaseFieldType IntegerType)
convertFieldType g str    = 
    if g `classNameExists` str
        then Right $ ClassFieldType str
        else Left $ ["Field of unknown type '" ++ str]
    

convertType :: Definitions -> CstType -> Error Type
convertType g (CstSimpleType s)  =
    case s of 
        "none"   -> Right $ BotType
        "bool"   -> Right $ (BaseType BoolType)
        "void"   -> Right $ (BaseType VoidType)
        "string" -> Right $ (BaseType StringType)
        "int"    -> Right $ (BaseType IntegerType)
        _        -> Left $ ["Unknown type " ++ s]
convertType g (CstClassType c u) = 
    if g `classNameExists` c
        then ClassType c <$> convertFullUsage u
        else Left $ ["Convert type failed could not find class " ++ c]
    

convertExpression :: Definitions -> CstExpression -> Error Expression
convertExpression g (CstExpressionPrint v)         = convertPrintExpression g v 
convertExpression g CstExpressionInput             = convertInputExpression g 
convertExpression g (CstExpressionNew name)        = convertNewExpression g name
convertExpression g (CstExpressionAssign fname e)  = convertAssignExpression g fname e
convertExpression g (CstExpressionCall r m v1 v2)  = convertCallExpression g r m v1 v2
convertExpression g (CstExpressionSeq e1 e2)       = convertSeqExpression g e1 e2
convertExpression g (CstExpressionIf e1 e2 e3)     = convertIfExpression g e1 e2 e3
convertExpression g (CstBinaryExpression op e1 e2) = convertBinaryExpression g op e1 e2
convertExpression g (CstExpressionLabel lbl e)     = convertLabelExpression g lbl e
convertExpression g (CstExpressionContinue lbl)    = convertContinueExpression g lbl
convertExpression g (CstExpressionValue val)       = convertValueExpression g val

convertValueExpression :: Definitions -> CstValue -> Error Expression
convertValueExpression g val = ExpressionValue <$> convertValue g val

convertValue :: Definitions -> CstValue -> Error Value 
convertValue g (CstBool b) = Right $ ValueBase (BaseBool b)
convertValue g (CstString s) = Right $ ValueBase (BaseString s)
convertValue g (CstInteger i) = Right $ ValueBase (BaseInteger i)
convertValue g CstUnit = Right $ ValueBase BaseUnit
convertValue g CstNull = Right $ ValueBase BaseNull
convertValue g (CstReference id) = ValueReference <$> convertReference g id

convertPrintExpression :: Definitions -> CstValue -> Error Expression
convertPrintExpression g v = ExpressionPrint <$> convertValue g v

convertInputExpression :: Definitions -> Error Expression
convertInputExpression g = return ExpressionInput 

convertNewExpression :: Definitions -> String -> Error Expression
convertNewExpression g name = 
    if classNameExists g name
        then Right $ ExpressionNew name
        else Left  $ ["New expression unknown class name " ++ name]

convertAssignExpression :: Definitions -> String -> CstExpression -> Error Expression
convertAssignExpression g fname e = 
    let e' = convertExpression g e
    in if fieldNameExists g fname
        then ExpressionAssign fname <$> e'
        else Left $ ["Assign expression unable to find field " ++ fname]

convertCallExpression :: Definitions -> String -> String -> CstValue -> CstValue -> Error Expression
convertCallExpression g r m v1 v2 = 
    let r'           = convertReference g r
        v1'          = convertValue g v1
        v2'          = convertValue g v2
        (l, rights)  = partitionEithers [v1', v2']
        [v1'', v2''] = rights
    in if null l
            then (\r'' -> ExpressionCall r'' m v1'' v2'') <$> r'
            else Left $ concat l
    
convertSeqExpression :: Definitions -> CstExpression -> CstExpression -> Error Expression
convertSeqExpression g e1 e2 = 
    let e1' = convertExpression g e1
        e2' = convertExpression g e2
    in combineErrors e1' e2' ExpressionSeq

convertIfExpression :: Definitions -> CstExpression -> CstExpression -> CstExpression -> Error Expression
convertIfExpression g e1 e2 e3 = 
    let e1'    = convertExpression g e1
        e2'    = convertExpression g e2
        e3'    = convertExpression g e3
        (l, r) = partitionEithers [e1', e2', e3']
        [e1'', e2'', e3''] = r
    in if null l
            then Right $ ExpressionIf e1'' e2'' e3''
            else Left $ concat l

convertBinaryExpression :: Definitions -> CstBinaryOperator -> CstExpression -> CstExpression -> Error Expression 
convertBinaryExpression g op e1 e2 = 
    let e1' = convertExpression g e1
        e2' = convertExpression g e2
        op' = convertOperator op
    in combineErrors e1' e2' (ExpressionBinaryOperation op')

convertOperator :: CstBinaryOperator -> BinaryOperator
convertOperator CstOpEQ = OpEQ
convertOperator CstOpNEQ = OpNEQ
convertOperator CstOpAnd = OpAnd
convertOperator CstOpOr = OpOr
convertOperator CstOpSub = OpSub
convertOperator CstOpAdd = OpAdd
convertOperator CstOpLT = OpLT
convertOperator CstOpGT = OpGT
convertOperator CstOpLEQ = OpLEQ
convertOperator CstOpGEQ = OpGEQ


convertLabelExpression :: Definitions -> String -> CstExpression -> Error Expression
convertLabelExpression g lbl e =
    let g' = g `insertLabel` lbl
    in if g `containsLabel` lbl
            then Left $ ["Label expression label " ++ lbl ++ " already exists"]
            else liftM (ExpressionLabel lbl) (convertExpression g' e)

convertContinueExpression :: Definitions -> String -> Error Expression
convertContinueExpression g lbl = 
    if g `containsLabel` lbl
        then Right $ ExpressionContinue lbl
        else Left $ ["Continue expression label " ++ lbl ++ " does not exist"]


convertReference :: Definitions -> String -> Error Reference
convertReference g r = 
    if g `fieldNameExists` r
        then Right $ RefField r
        else if g `parameterExists` r
                then Right $ RefParameter r
                else Left $ ["Unable to find field or parameter " ++ r]



convertFullUsage :: CstUsage -> Error Usage
convertFullUsage u = (\u' -> Usage u' SplitEmpty) <$> convertUsage u []

convertUsage :: CstUsage -> [String] -> Error UsageImpl
convertUsage (CstUsageChoice u1 u2) rec = 
    let u1' = convertUsage u1 rec
        u2' = convertUsage u2 rec
    in combineErrors u1' u2' UsageChoice 
convertUsage (CstUsageBranch lst)   rec = 
    let lst'   = map (\(s, u) -> (s, convertUsage u rec)) lst  :: [(String, Error UsageImpl)]
        lst''  = map (\(s, u) -> fmap (\u' -> (s, u')) u) lst' :: [Error (String, UsageImpl)]
        (l, r) = partitionEithers lst''
    in if null l
            then Right $ UsageBranch r
            else Left  $ concat l
convertUsage (CstUsageParallel u1 u2 u3) rec =
    let u1'    = convertUsage u1 []  -- we should not know outside recursive variables 
        u2'    = convertUsage u2 []
        u3'    = convertUsage u3 rec
        (l, r) = partitionEithers [u1', u2', u3']
        [u1'', u2'', u3''] = r
    in if null l
            then Right $ UsageParallel u1'' u2'' u3''
            else Left $ concat l
convertUsage (CstUsageRecursive r u) rec = 
    if r `elem` rec
        then Left ["Error redefinition of recursive variable " ++ r]
        else UsageRecursive r <$> convertUsage u (rec ++ [r])
convertUsage (CstUsageVariable r) rec =
    if r `notElem` rec
        then Left ["Error recursive variable " ++ r ++ " not defined"]
        else Right $ UsageVariable r
convertUsage CstUsageEnd rec = Right UsageEnd



checkUsage :: Usage -> [String] -> [String]
checkUsage u methodNames = 
    let calledMethods    = usageMethods (currentUsage u)
        undefinedMethods = [ "usage " ++ show (currentUsage u) ++ " tries to call " ++ name ++ " but it does not exists" 
                           | name <- calledMethods, name `notElem` methodNames]
    in undefinedMethods

usageMethods :: UsageImpl -> [String]
usageMethods (UsageChoice u1 u2)      = usageMethods u1 ++ usageMethods u2
usageMethods (UsageBranch lst)        = map fst lst ++ concat (map (usageMethods . snd) lst)
usageMethods (UsageRecursive _ u)     = usageMethods u
usageMethods (UsageVariable _)        = []
usageMethods (UsageParallel u1 u2 u3) = usageMethods u1 ++ usageMethods u2 ++ usageMethods u3
usageMethods (UsageEnd)               = []


