module TypeSystem where

import Ast
import StateErrorMonad
import Data.Either (rights, isRight, fromRight, partitionEithers)
import Data.Maybe (isNothing, catMaybes, isJust, fromJust)
import Data.List (permutations, sortBy, nub, intersect)
import Control.Monad (forM, when, unless)
import Control.Arrow (second)

-- debug stuff
import Debug.Trace
debugTrace s = trace s $ return ()

type Gamma = [(String, Type)]

(===) :: Gamma -> Gamma -> Bool
(===) g1 g2 =  
    let sortFunction = sortBy (\a b -> fst a `compare` fst b) 
    in (sortFunction g1) == (sortFunction g2)

data Omega  = Omega [(String, Gamma)] 
    deriving (Show)

data Environments = Environments { gamma :: Gamma 
                                 , omega :: Omega
                                 } deriving (Show)

data ClassData = ClassData { allClasses   :: [Class]
                           , currentClass :: String 
                           } deriving (Show)

data MyState = MyState { environments :: Environments
                       , classData    :: ClassData 
                       }
             | MyStateAny
                deriving (Show)

isAnyState :: MyState -> Bool
isAnyState MyStateAny = True
isAnyState _          = False

type NDTypeSystem a = MState [(MyState, Type)] a
type DTypeSystem  a = MState (MyState, Type) a

instance Eq MyState where
    MyStateAny == _          = True
    _          == MyStateAny = True
    s1 == s2 =  
        let g1 = gamma $ environments s1
            g2 = gamma $ environments s2
        in g1 === g2


forAll :: DTypeSystem [(MyState, Type)] -> NDTypeSystem ()
forAll f = do
    states <- getState
    let states' = nub $ concat $ map fst $ rights $ map (runState f) states
    when (null states') $ fail "" 
    rewriteStates states' 
    return ()

-- tpying class usage 
type RecursiveEnv = [(String, Gamma)]

data UsageState = UsageState { currentGamma     :: Gamma 
                             , currentRecursive :: RecursiveEnv
                             , classInfo        :: ClassData 
                             } 
                | UsageStateAny deriving (Show)


isUsageState (UsageStateAny) = False 
isUsageState _               = True

instance Eq UsageState where
    a == UsageStateAny = True
    UsageStateAny == b = True
    a == b             = currentGamma a === currentGamma b

type DUsageState a  = MState UsageState a
type NDUsageState a = MState [UsageState] a

forAll' :: DUsageState [UsageState] -> NDUsageState ()
forAll' f = do
    states <- getState
    let states' = filter isUsageState states
    let states'' = map (runState f) states'
    let states''' = concat $ map fst $ rights $  states''
    when (not (null states')) $ rewriteStates' states'''
    when (null states''') $ fail ""
    return ()

rewriteStates' :: [UsageState] -> NDUsageState ()
rewriteStates' ls = MState $ \_ -> Right ((), ls)

validEnvironments :: Eq a => [a] -> [[a]] -> [a]
validEnvironments (x:xs) ls = if all (x `elem`) ls
                                    then x : validEnvironments xs ls
                                    else validEnvironments xs ls
validEnvironments []     ls = []

usefulUsageStates :: [[UsageState]] -> [UsageState]
usefulUsageStates states =  
    let states'  = concat states
        states'' = filter isUsageState states' 
    in if null states'' && length states' > length states''
            then [UsageStateAny]
            else states''


-- monad looking into functions

rewriteStates :: [(MyState, Type)] -> NDTypeSystem ()
rewriteStates ls = MState $ \_ -> Right ((), usefulStates ls)

usefulStates :: [(MyState, Type)] -> [(MyState, Type)]
usefulStates s = 
    let s' = filter (not . isAnyState . fst) s
    in if null s' then s else s'

getMyState ::  DTypeSystem MyState
getMyState = MState $ \m -> Right (fst m, m)

getReturnType :: DTypeSystem Type 
getReturnType = MState $ \m -> Right (snd m, m)

envApply :: (Environments -> a) -> DTypeSystem a
envApply f = MState $ \m -> Right $ (f (environments (fst m)), m)

getGamma :: DTypeSystem Gamma
getGamma = envApply gamma

getOmega :: DTypeSystem Omega
getOmega = envApply omega

getClassData :: DTypeSystem ClassData
getClassData = MState $ \m -> Right (classData (fst m), m)

getCurrentClassName :: DTypeSystem String
getCurrentClassName = currentClass <$> getClassData

getAllClasses :: DTypeSystem [Class]
getAllClasses = allClasses <$> getClassData

findClass :: String -> DTypeSystem Class
findClass name = do
    classes <- getAllClasses
    let found = filter ((name ==) . className) classes
    headM found

getField :: FieldName -> DTypeSystem FieldType
getField fieldname = do
    clazz <- getCurrentClassName >>= findClass 
    field <- headM $ filter (\f -> fieldName f == fieldname) $ classFields clazz
    return $ fieldType field

getClassByReference :: Reference -> DTypeSystem Class
getClassByReference ref = do
    gamma <- getGamma
    let refName = getReferenceName ref
    let res = filter ((refName ==) . fst) gamma
    t <- snd <$> headM res
    className <- classNameFromType t
    findClass className
    where classNameFromType (ClassType cname _) = return cname
          classNameFromType _                   = fail "not found"

getMethod :: Reference -> String -> DTypeSystem Method
getMethod ref mname = do
    cls <- getClassByReference ref
    let methods = classMethods cls
    let method = filter ((mname ==) . methodName) methods
    headM method

writeGamma :: Gamma -> DTypeSystem ()
writeGamma g = MState $ \(s, t) ->
    let s' = updateGammaInState s g
    in Right ((), (s', t))

extractTypeFromGamma :: String -> DTypeSystem Type
extractTypeFromGamma fieldname = do
    --let count = length $ filter ((fieldname ==) . fst) gamma
    --assert' $ count == 1
    gamma <- getGamma
    let withName    = filter ((fieldname ==) . fst) gamma
    let withoutName = filter ((fieldname /=) . fst) gamma
    
    t <- headM withName
    let withName' = tail withName

    let gamma' = withName' ++ withoutName
    when (isClassType (snd t)) $ writeGamma gamma'
    return $ snd t

extractTypeFromValue :: Value -> DTypeSystem Type
extractTypeFromValue (ValueBase b)        = return $ baseValueToType b
extractTypeFromValue (ValueReference ref) = extractTypeFromGamma $ getReferenceName ref

baseValueToType :: BaseValue -> Type
baseValueToType bv = 
    case bv of 
        BaseUnit       -> (BaseType VoidType)
        BaseNull       -> BotType
        (BaseBool _)   -> (BaseType BoolType)
        (BaseString _) -> (BaseType StringType)
        (BaseInteger _) -> (BaseType IntegerType)

updateNameInGamma :: String -> Type -> DTypeSystem ()
updateNameInGamma name type' = do 
    gamma <- getGamma
    let gamma' = (name, type') : gamma    
    when (isClassType type') $ writeGamma gamma'
    return ()  

updateValueInGamma :: Value -> Type -> DTypeSystem () 
updateValueInGamma (ValueBase      bv)  type' = return ()
updateValueInGamma (ValueReference ref) type' = updateNameInGamma (getReferenceName ref) type' 

updateGammaInState :: MyState -> Gamma -> MyState
updateGammaInState state newGamma = 
    let env  = environments state
        env' = env { gamma = newGamma }
    in state { environments = env' }

updateOmegaInState :: MyState -> Omega -> MyState
updateOmegaInState state newOmega = 
    let env  = environments state
        env' = env { omega = newOmega }
    in state { environments = env' }

typeExtractClassInfo :: MonadFail m => Type -> m (String, Usage)
typeExtractClassInfo (ClassType cn u) = return (cn, u)
typeExtractClassInfo _                = fail "not a class type"

isClassType :: Type -> Bool
isClassType BotType         = True
isClassType (ClassType _ _) = True 
isClassType _               = False

isBoolType :: Type -> Bool
isBoolType (BaseType BoolType) = True
isBoolType _          = False


getClass' :: DUsageState Class 
getClass' = do
    s <- getState
    let info    = classInfo s 
        name    = currentClass info
        classes = allClasses info
    cls <- headM $ filter ((name ==) . className) classes 
    return cls

getMethod' :: String -> DUsageState Method
getMethod' name = do
    cls <- getClass'
    let methods = classMethods cls
    let method = filter ((name == ) . methodName) methods
    headM method

assertNotAnyState :: DTypeSystem ()
assertNotAnyState = do
    state' <- getMyState
    when (isAnyState state') $ fail ""
    return ()
---

term :: Type -> Bool
term BotType         = True
term (BaseType t)    = True
term (ClassType _ u) = term' (currentUsage u)
    where term' :: UsageImpl -> Bool
          term' (UsageEnd) = True
          term' _          = False

agree :: FieldType -> Type -> Bool
agree (BaseFieldType t1)   (BaseType t2)     = t1 == t2
agree (ClassFieldType cn1) (ClassType cn2 u) = cn1 == cn2 
agree (ClassFieldType cn1) (BotType)         = True
agree _                    _                 = False

methodTransitions :: Usage -> [(String, Usage)]
methodTransitions u = methodTransitions' [] u
    where
        methodTransitions' :: [String] -> Usage -> [(String, Usage)]
        methodTransitions' recU (Usage (UsageChoice u1 u2) s)  = []
        methodTransitions' recU (Usage (UsageBranch lst) s)    = map (\(mname, uimpl) -> (mname, Usage uimpl s)) lst
        methodTransitions' recU (Usage (UsageRecursive x u) s) = 
            if x `notElem` recU 
                then methodTransitions' (x:recU) (Usage (substituteUsage (UsageRecursive x u) x u) s)
                else []
        methodTransitions' recU (Usage (UsageVariable x) s)    = [] -- should never be reached
        methodTransitions' recU (Usage (UsageParallel _ _ _) s)= [] -- we cannot do transitions directly on parallel usages
        methodTransitions' recU (Usage (UsageEnd) s)           = [] 
        methodTransitions' recU (Usage (UsagePlaceholder) s)   = [] -- should never be reached

choiceTransitions :: MonadFail m => Usage -> m (Usage, Usage)
choiceTransitions u = 
    case currentUsage u of
        (UsageChoice u1 u2)  -> return (u {currentUsage = u1} , u{currentUsage = u2})
        _                    -> fail "not a choiceUsage"


checkExpression :: Expression -> NDTypeSystem ()
checkExpression (ExpressionPrint v)                  = checkPrint v 
checkExpression ExpressionInput                      = checkInput
checkExpression (ExpressionSeq e1 e2)                = checkSeq e1 e2
checkExpression (ExpressionAssign f e)               = checkFld f e
checkExpression (ExpressionCall r m v1 v2)           = checkCall r m v1 v2
checkExpression (ExpressionValue v)                  = checkVal v
checkExpression (ExpressionNew name)                 = checkNew name
checkExpression (ExpressionIf e1 e2 e3)              = checkIf e1 e2 e3
checkExpression (ExpressionLabel lbl e)              = checkLab lbl e
checkExpression (ExpressionContinue lbl)             = checkCon lbl
checkExpression (ExpressionBinaryOperation op e1 e2) = checkBinaryExpression op e1 e2

checkPrint :: Value -> NDTypeSystem ()
checkPrint v = forAll $ do
    s <- getMyState
    return [(s, (BaseType VoidType))]

checkInput :: NDTypeSystem ()
checkInput = forAll $ do
    s <- getMyState
    return [(s, (BaseType StringType))]

checkSeq :: Expression -> Expression -> NDTypeSystem ()
checkSeq e1 e2 = do
    checkExpression e1
    forAll $ do 
        assertNotAnyState
        s <- getState
        t <- getReturnType
        assert' $ term t
        return [s]
    checkExpression e2

checkFld :: FieldName -> Expression -> NDTypeSystem ()
checkFld fieldname expression = do
    checkExpression expression
    forAll $ do
        assertNotAnyState
        t <- getReturnType
        f <- getField fieldname
        assert' $ agree f t
        t' <- extractTypeFromGamma fieldname 
        assert' $ term t'
        updateNameInGamma fieldname t 
        gammaResult <- getGamma
        state <- getMyState
        let state' = updateGammaInState state gammaResult
        return [(state', (BaseType VoidType))]

checkCall :: Reference -> MethodName -> Value -> Value -> NDTypeSystem ()
checkCall r m v1 v2 = do
    -- find the names of r v1 v2
    let names = getReferenceName r : catMaybes [ getValueName v1, getValueName v2]
    -- split gamma
    -- check call
    -- unsplit gamma
    splitGamma names

    forAll $ do
        assertNotAnyState
        -- find the method definition
        (Method retType _ p1 p2 _) <- getMethod r m 
        t <- extractTypeFromGamma $ getReferenceName r 
        -- convert type to Usage
        (cn, u) <- typeExtractClassInfo t
        -- check that the reference can do the call
        let transitions = methodTransitions u
        -- find the usages we can end up in 
        let resultingUsages = map snd $ filter ((== m) . fst) transitions
        
        let (Parameter fromTypeP1 toTypeP1 _) = p1
        let (Parameter fromTypeP2 toTypeP2 _) = p2
        
        v1Type <- extractTypeFromValue v1
        v2Type <- extractTypeFromValue v2
        --getGamma >>= \g -> debugTrace ("post extract gamma " ++ show g)

        -- check that parameters match
        -- TODO we should only check usageimpl
        when (isClassType v1Type && isClassType  fromTypeP1) $ do
            (cnV1, uV1) <- typeExtractClassInfo v1Type
            (cnFrom1, uFrom1) <- typeExtractClassInfo fromTypeP1
            assert' $ cnV1 == cnFrom1 && currentUsage uFrom1 == currentUsage uV1

        unless (isClassType v1Type && isClassType fromTypeP1) $ do
            assert' $ fromTypeP1 == v1Type

        when (isClassType v2Type && isClassType fromTypeP2) $ do
            (cnV2, uV2) <- typeExtractClassInfo v2Type
            (cnFrom2, uFrom2) <- typeExtractClassInfo fromTypeP2
            assert' $ cnV2 == cnFrom2 && currentUsage uFrom2 == currentUsage uV2

        unless (isClassType v1Type && isClassType  fromTypeP1) $ do
            assert' $ fromTypeP2 == v2Type
        
        -- save the resulting parameter type 
        -- we should save the previous s
        when (isClassType v1Type && isClassType toTypeP1) $ do
            -- copy the s
            (cnV1, uV1)   <- typeExtractClassInfo v1Type
            (cnTo1, uTo1) <- typeExtractClassInfo toTypeP1
            let u' = currentUsage uTo1
            let newUsage = uV1 { currentUsage = u' }
            assert' $ cnV1 == cnTo1 
            updateValueInGamma v1 (ClassType cnV1 newUsage) 

        unless (isClassType toTypeP1) $ do
            updateValueInGamma v1 toTypeP1

        when (isClassType v2Type && isClassType toTypeP2) $ do
            -- copy the s
            (cnV2, uV2)   <- typeExtractClassInfo v2Type
            (cnTo2, uTo2) <- typeExtractClassInfo toTypeP2
            let u' = currentUsage uTo2
            let newUsage = uV2 { currentUsage = u' }
            assert' $ cnV2 == cnTo2 
            updateValueInGamma v2 (ClassType cnV2 newUsage) 

        unless (isClassType toTypeP2) $ do
            updateValueInGamma v2 toTypeP2

        --getGamma >>= \g -> debugTrace ("updated gamma " ++ show g)

        gamma <- getGamma

        s <- getMyState
        let res = do u' <- resultingUsages
                     let gamma' = ((getReferenceName r), (ClassType cn u')) : gamma
                     let s' = updateGammaInState s gamma'
                     return (s', retType)
        assert' $ not (null res)
        return res
        
    unsplitGamma names

    where getValueName :: Value -> Maybe String
          getValueName (ValueReference r) = Just $ getReferenceName r
          getValueName _                  = Nothing
            
getReferenceName :: Reference -> String
getReferenceName (RefParameter n) = n
getReferenceName (RefField n)     = n

splitGamma :: [String] -> NDTypeSystem ()
splitGamma variablesIn = do 
    st <- getState
    --debugTrace $ "splitGamma ND states " ++ show (length st)
    forAll $ do
            --debugTrace $ "split start " ++ show variablesIn
            gamma <- getGamma
            let gammas = splitGamma' [[]] (reverse gamma)
            s <- getMyState
            t <- getReturnType
            --debugTrace $ "split size: " ++ show (length gammas)
            --debugTrace $ "splitLast " ++ show (last gammas)
            return [ (updateGammaInState s gamma', t) 
                   | gamma' <- gammas]

    --st' <- getState
    --debugTrace $ "splitGamma ND states - done " ++ show (length st')
    where variables = nub variablesIn
          splitGamma' :: [Gamma] -> Gamma -> [Gamma]
          splitGamma' gammas []            = gammas
          splitGamma' gammas ((f, t) : gs) =
                if f `elem` variables
                    then splitGamma' [ field ++ g | g <- gammas, field <- splitField (f, t) ] gs
                    else splitGamma' [ (f, t) : g | g <- gammas] gs

splitField :: (String, Type) -> [[(String, Type)]]
splitField (f, (ClassType cn u)) = 
    let split     = splitUsage u :: [[Usage]]
        withType  = map (map convertToType) split :: [[Type]]
        withField = map (map addField) withType :: [[(String, Type)]]
    in withField
        where convertToType u' = ClassType cn u'
              addField t       = (f, t)
splitField (f, t) = [[(f, t)]]

splitUsage :: Usage -> [[Usage]]
splitUsage u =
    let split             = splitUsage' u
        splitPermutations = concat [ permutations lst | lst <- split]
    in splitPermutations
    where
        splitUsage' :: Usage -> [[Usage]]
        splitUsage' u@(Usage (UsageParallel u1 u2 u3) s) = 
            let u1' = Usage u1 (SplitLeft s)
                u2' = Usage u2 (SplitRight s)
                u3' = Usage (UsageParallel UsagePlaceholder UsagePlaceholder u3) s
                u1Split = splitUsage' u1' :: [[Usage]]
                u2Split = splitUsage' u2' :: [[Usage]]
            in [[u]] ++ [[u3'] ++ lst1 ++ lst2 | lst1 <- u1Split
                                               , lst2 <- u2Split]
        splitUsage' u = [[u]]

unsplitGamma :: [String] -> NDTypeSystem ()
unsplitGamma variablesIn = forAll $ do
    let variables = nub variablesIn
    gamma <- getGamma

    -- find the all the references with variable name
    -- we cannot assume ClassType as it could be anything currently
    let sharedVariable = [ (name, sharedTypes)
                         | name <- variables
                         , let shared       = filter ((name ==) . fst) gamma
                         , let sharedTypes  = map snd shared] :: [(String, [Type])]

    let variables' = [ name
                     | (name, sharedTypes) <- sharedVariable
                     , length sharedTypes > 1 ] :: [String]

    -- we only care about variables with more than 1 type
    let sharedVariable' = [ (name, sharedTypes)
                          | (name, sharedTypes) <- sharedVariable
                          , length sharedTypes > 1 ] :: [(String, [Type])]

    -- TODO could just be a BotType
    let sharedUsage = [ (name, sharedUsages)
                    | (name, sharedTypes) <- sharedVariable'
                    , let sharedUsages = map snd $ catMaybes $ map typeExtractClassInfo sharedTypes ] :: [(String, [Usage])]


    assert' $ length sharedVariable' == length sharedUsage

    -- TODO make sure that the classType is the same
    let combined = [ (f, fixPointCombiner usages) 
                   | (f, usages) <- sharedUsage ] :: [(String, Maybe [Usage])]
    
   
    -- remove all combinations which failed
    let combined' = map (second fromJust) $ filter (isJust . snd) combined

    
    -- we should not have lost any field
    assert' $ length combined' == length combined

    -- check that all fields have one type
    let combined'' = map (second head) $ filter ((1 ==) . length . snd) combined'

    -- we should still not have lost anything
    assert' $ length combined'' == length combined

    -- combine back into types
    let combined''' = [ (f, (ClassType cn u))
                      | (f, u) <- combined'' 
                      , let (_, (ClassType cn _)) = head $ filter ((f ==) . fst) gamma]

    -- update gamma
    let gamma'  = filter ((`notElem` variables') . fst) gamma
    let gamma'' = gamma' ++ combined'''
    -- sort gamma''
    let gamma''' = map fixEndParallelEndType $ sortBy (\a b -> fst a `compare` fst b ) gamma''

    -- if not possible remove it
    -- test that we only have 1 of each type
    let variableCount = map fst gamma'''
    assert' $ length variableCount == length (nub variableCount)

    s <- getMyState
    t <- getReturnType
    let s' = updateGammaInState s gamma'''
    
    return [(s',t)]
    where fixEndParallelEndType :: (String, Type) -> (String, Type) 
          fixEndParallelEndType (f, (ClassType cn u)) = (f, (ClassType cn (fixEndParallelEnd u)))
          fixEndParallelEndType otherwise             = otherwise
        
          fixEndParallelEnd :: Usage -> Usage
          fixEndParallelEnd u = u{currentUsage = fixEndParallelEnd' (currentUsage u)}

          fixEndParallelEnd' :: UsageImpl -> UsageImpl
          fixEndParallelEnd' (UsageParallel UsageEnd UsageEnd u) = u
          fixEndParallelEnd' (UsageParallel u1 u2 u3) = 
                let u1' = (fixEndParallelEnd' u1)
                    u2' = (fixEndParallelEnd' u2)
                in if u1' == UsageEnd && u2' == UsageEnd
                        then u3
                        else UsageParallel u1' u2' u3
          fixEndParallelEnd' u = u
 
          fixPointCombiner :: [Usage] -> Maybe [Usage]
          fixPointCombiner lst = do
              lst' <- combiner [] lst
              if lst == lst'
                  then Just lst'
                  else fixPointCombiner lst'
  
          combiner :: [Usage] -> [Usage] -> Maybe [Usage]
          combiner ul []     = return ul
          combiner ul (u:xs) = do
              let o = UsagePlaceholder
              let s = currentSplit u
              let u' = currentUsage u
              case u' of 
                  (UsageParallel UsagePlaceholder UsagePlaceholder u3) -> 
                      combiner' ul xs u3 s
                  _ -> 
                      combiner (ul ++ [u]) xs
  
          combiner' :: [Usage] -> [Usage] -> UsageImpl -> SplitUsage -> Maybe [Usage]
          combiner' ul xs u3 s = do
              let rhs = filter ((SplitRight s ==) . currentSplit) $ ul ++ xs
              let lhs = filter ((SplitLeft s ==) . currentSplit) $ ul ++ xs
              let ul' = filter (\u -> currentSplit u `notElem` [SplitLeft s, SplitRight s]) $ ul
              let xs' = filter (\u -> currentSplit u `notElem` [SplitLeft s, SplitRight s]) $ xs
              assert' $ length rhs == 1
              assert' $ length lhs == 1
              let ru = head rhs
              let lu = head lhs
              -- make sure that ru and lu is completed
              let completed = (Usage (UsageParallel (currentUsage lu) (currentUsage ru) u3) s)
              let incomplet = (Usage (UsageParallel UsagePlaceholder UsagePlaceholder u3) s)
              if isComplete ru && isComplete lu
                  then combiner (ul' ++ [completed]) xs'
                  else combiner (ul ++ [incomplet]) xs

          isComplete :: Usage -> Bool
          isComplete (Usage (UsageParallel UsagePlaceholder UsagePlaceholder _) _) = False
          isComplete _                                                             = True


checkVal :: Value -> NDTypeSystem ()
checkVal (ValueBase bv)     = checkBaseVal bv
checkVal (ValueReference r) = checkRef r

checkBaseVal :: BaseValue -> NDTypeSystem ()
checkBaseVal bv = forAll $ do
    assertNotAnyState
    s <- getMyState
    let t = baseValueToType bv
    return [(s, t)]

checkRef :: Reference -> NDTypeSystem ()
checkRef r = forAll $ do
    assertNotAnyState
    let name = case r of
                    (RefParameter name) -> name
                    (RefField name)     -> name
    t <- extractTypeFromGamma name 
    -- only when is not base type
    if (isClassType t) 
        then updateNameInGamma name BotType
        else updateNameInGamma name t
    gammaResult <- getGamma
    state <- getMyState
    let state' = updateGammaInState state gammaResult
    return [(state', t)]

checkNew :: ClassName -> NDTypeSystem ()
checkNew name = forAll $ do
    assertNotAnyState
    currentState <- getMyState
    cls <- findClass name
    let u = classUsage cls
    let t = ClassType name u
    return [(currentState, t)]

checkIf :: Expression -> Expression -> Expression -> NDTypeSystem ()
checkIf e1 e2 e3 = do
    checkExpression e1 
    (ExpressionCall r m v1 v2) <- toCallExpression e1
    let name = getReferenceName r
    splitGamma [name]
    forAll $ do
        assertNotAnyState
        retType <- getReturnType
        assert' $ isBoolType retType 
        t <- extractTypeFromGamma name
        (cn, u) <- typeExtractClassInfo t
        (usageT, usageF) <- choiceTransitions u
        myState <- getMyState
        gamma' <- getGamma
        let gammaTrue    = (name, (ClassType cn usageT)) : gamma'
        let gammaFalse   = (name, (ClassType cn usageF)) : gamma'
        let myStateTrue  = updateGammaInState myState gammaTrue
        let myStateFalse = updateGammaInState myState gammaFalse
        (_,  trueStates) <- fromEitherM $ runState (checkExpression e2) [(myStateTrue, (BaseType VoidType))]
        let r = runState (checkExpression e3) [(myStateFalse, (BaseType VoidType))]
        (_, falseStates) <- fromEitherM $ runState (checkExpression e3) [(myStateFalse, (BaseType VoidType))]
        let res = do (trueEnvs,  trueType)  <- trueStates  
                     (falseEnvs, falseType) <- falseStates 
                     assert' $ trueType == falseType
                     assert' $ trueEnvs == falseEnvs
                     let anyState = (isAnyState trueEnvs && isAnyState falseEnvs) 
                     let envs = if isAnyState trueEnvs
                                     then falseEnvs
                                     else trueEnvs
                     -- we only want to save gamma
                     let newGamma = gamma $ environments envs
                     let myState' = updateGammaInState myState newGamma
                     if anyState 
                            then return (MyStateAny, trueType)
                            else return (myState', trueType)
        assert' $ not (null res)
        return res

    unsplitGamma [name]
    where toCallExpression e@(ExpressionCall _ _ _ _) = return e
          toCallExpression _                          = fail "not a call expression"


checkLab :: LabelName -> Expression -> NDTypeSystem ()
checkLab lbl e = do 
    forAll $ do
        assertNotAnyState
        (Omega lbls) <- getOmega
        let found = lbl `envLookupIn` lbls
        assert' (isNothing found)
        gamma <- getGamma
        let lbls' = (lbl,gamma):lbls
        state <- getMyState 
        let state' = updateOmegaInState state (Omega lbls')
        return [(state', (BaseType VoidType))]
    checkExpression e

checkCon :: LabelName -> NDTypeSystem ()
checkCon lbl = forAll $ do
    assertNotAnyState
    (Omega lbls) <- getOmega
    expectedGamma <- lbl `envLookupIn` lbls
    gamma <- getGamma
    assert' $ expectedGamma === gamma 
    return [(MyStateAny, (BaseType VoidType))]

convertNDToD :: NDTypeSystem () -> DTypeSystem [(MyState, Type)]
convertNDToD nd = do 
    s <- getState
    (a, newStates) <- fromEitherM $ runState nd [s]
    return $ newStates


checkBinaryExpression :: BinaryOperator -> Expression -> Expression -> NDTypeSystem ()
checkBinaryExpression op e1 e2 = do
    let types = operatorType op
    let thd (x, y, z) = z
    let findType t t' = 
            map thd $ filter (\(t1, t1', t1'') -> t == t1 && t' == t1') types
    checkExpression e1
    forAll  $ do
        t <- getReturnType
        ss <- convertNDToD $ checkExpression e2
        let res = [ (s, t'') 
                  | (s, t') <- ss, 
                    t'' <- findType t t'
                  ]
        assert' $ not (null res)
        return res


operatorType :: BinaryOperator -> [(Type, Type, Type)]
operatorType o = operatorType' o (BaseType BoolType) (BaseType StringType) (BaseType IntegerType)

operatorType' o b s i
    | o `elem` [OpAnd, OpOr]              = [(b, b, b)]
    | o `elem` [OpAdd, OpSub]             = [(i, i, i)]
    | o `elem` [OpEQ, OpNEQ]              = [(s, s, b),
                                             (b, b, b),
                                             (i, i, b)]
    | otherwise                           = []


checkTUsage :: UsageImpl -> NDUsageState ()
checkTUsage usage =
    case usage of
        (UsageChoice u1 u2)      -> checkTCCh u1 u2
        (UsageBranch lst)        -> checkTCBr lst
        (UsageRecursive x u)     -> checkTCRec x u
        (UsageVariable x)        -> checkTCVar x
        (UsageParallel u1 u2 u3) -> checkTCPar u1 u2 u3
        (UsageEnd)               -> checkTCEn
        (UsagePlaceholder)       -> fail "should never appear in a class definition"

checkTCCh :: UsageImpl -> UsageImpl -> NDUsageState ()
checkTCCh u1 u2 = forAll' $ do
    s <- getState
    (_, s1) <- fromEitherM $ runState (checkTUsage u1) [s]
    (_, s2) <- fromEitherM $ runState (checkTUsage u2) [s]
    let result = [s1, s2]
    let resultFirst = usefulUsageStates result
    let resultFinal = validEnvironments resultFirst result    
    assert' $ not (null resultFinal)
    return resultFinal

checkTCBr :: [(String, UsageImpl)] -> NDUsageState ()
checkTCBr lst = forAll' $ do
    assert' $ not (null lst)
    let res = map (uncurry checkTCBr') lst 
    s <- getState
    let res' = map (\ndstate -> runState ndstate [s]) res
    assert' $ all isRight res'
    let res'' = map snd $ rights res'
    let resFirst = usefulUsageStates res''
    let resTail = res''
    let result = validEnvironments resFirst resTail
    assert' $ not (null result)
    return result

checkTCBr' :: String -> UsageImpl -> NDUsageState ()
checkTCBr' lbl uimpl = forAll' $ do

    (Method rettype _ p1 p2 e) <- getMethod' lbl
    let (Parameter p1FromType p1ToType x1) = p1 
    let (Parameter p2FromType p2ToType x2) = p2
    s <- getState
    -- insert the parameter type
    let gamma = currentGamma s ++ [(x1, p1FromType), (x2, p2FromType)]
    let omega = Omega $ currentRecursive s
    let env = Environments gamma omega
    let classData = classInfo s
    let myState = MyState env classData
    let res = runState (checkExpression e) [(myState, BotType)]
    res' <- snd <$> fromEitherM res
    let res'' = do (myState', ti) <- res'
                   let (Environments gamma' omega') = environments myState'
                   -- pull the parameter type out
                   p1Type <- x1 `envLookupIn` gamma'
                   p2Type <- x2 `envLookupIn` gamma'
                   -- check they match expected type
                   assert' $ p1Type == p1ToType
                   assert' $ p2Type == p2ToType

                   -- remove them
                   let gamma'' = filter (\(r, _) -> r /= x1 && r /= x2) gamma'
                   -- check return type match
                   assert' $ ti == rettype
                   let s' = s { currentGamma = gamma'' }
                   -- run the rest of checkTUSage
                   finalRes <- fromEitherM $ runState (checkTUsage uimpl) [s']
                   snd finalRes
    assert' $ not (null res'')
    return res''
        
checkTCRec :: String -> UsageImpl -> NDUsageState ()
checkTCRec x u = forAll' $ do
    s <- getState
    let rec = currentRecursive s 
    assert' $ isNothing (x `lookup` rec)
    let rec' = (x, currentGamma s) : rec
    let s' = s { currentRecursive = rec'}
    setState s'
    result <- convertNDToD' $ checkTUsage u
    return result
    where setState s = MState $ \m -> Right ((), s)
          convertNDToD' :: NDUsageState () -> DUsageState [UsageState]
          convertNDToD' nd = do 
              s <- getState
              if isUsageState s
                  then do (_, newStates) <- fromEitherM $ runState nd [s]
                          return $ newStates
                  else return [s]

checkTCVar :: String -> NDUsageState ()
checkTCVar x = forAll'  $ do
    s <- getState
    let rec = currentRecursive s
    let gamma = currentGamma s 
    oldGamma <- x `envLookupIn` rec
    assert' $ gamma === oldGamma
    return [UsageStateAny]

checkTCPar :: UsageImpl -> UsageImpl -> UsageImpl -> NDUsageState ()
checkTCPar u1 u2 u3 = forAll' $ do 
    cls <- getClass' 
    -- lookup all fields required to follow u1 and u2
    let fieldsU1 = nub $ findFields cls u1
    let fieldsU2 = nub $ findFields cls u2
    
    -- check they are disjoint
    assert' $ fieldsU1 `intersect` fieldsU2 == []

    -- check each part with their respective small gamma
    (gamma1, gamma2) <- splitGammaInTwo fieldsU1

    -- check the two constituens with the smaller gamma
    clsInfo <- classInfo <$> getState
    let u1State = UsageState gamma1 [] clsInfo
    let u2State = UsageState gamma2 [] clsInfo

    let res1 = runState (checkTUsage u1) [u1State]
    let res2 = runState (checkTUsage u2) [u2State]

    -- [UsageState]
    (_, res1') <- fromEitherM res1
    -- [UsageState]
    (_, res2') <- fromEitherM res2 

    -- combine them into one gamma
    let gammas = do gamma1' <- currentGamma <$> res1'
                    gamma2' <- currentGamma <$> res2'
                    return $ sortBy (\a b -> fst a `compare` fst b) $ gamma1' ++ gamma2'

    currentState <- getState
    -- [UsageState]
    let newStates =  map (\gamma' -> currentState {currentGamma = gamma'}) gammas
    
    fromEitherM $ snd <$> runState (checkTUsage u3) newStates

splitGammaInTwo :: [String] -> DUsageState (Gamma, Gamma)
splitGammaInTwo flds = do
    s <- getState 
    let gamma = currentGamma s
    let gamma1 = filter ((`elem` flds) . fst) gamma
    let gamma2 = filter ((`notElem` flds) . fst) gamma
    assert' $ (map fst gamma1) `intersect` (map fst gamma2) == []
    return (gamma1, gamma2)

findFields :: Class -> UsageImpl -> [String]
findFields cls (UsageChoice u1 u2)      = nub $ findFields cls u1 ++ findFields cls u2
findFields cls (UsageBranch lst)        = nub $ findFields' cls (map fst lst) ++ concat (map (findFields cls . snd) lst)
findFields cls (UsageRecursive _ u)     = findFields cls u
findFields cls (UsageVariable _)        = []
findFields cls (UsageParallel u1 u2 u3) = nub $ findFields cls u1 ++ findFields cls u2 ++ findFields cls u3
findFields cls (UsageEnd)               = []
findFields cls (UsagePlaceholder)       = []

findFields' :: Class -> [String] -> [String]
findFields' cls methods = concat $ map (findFields'' cls) methods

findFields'' :: Class -> String -> [String]
findFields'' cls name = 
    let methods = classMethods cls
        method  = filter ((name ==) . methodName) methods
    in if null method 
            then []
            else extractFields (body (head method))

extractFields :: Expression -> [String]
extractFields (ExpressionSeq e1 e2)      = nub $ extractFields e1 ++ extractFields e2
extractFields (ExpressionAssign f e)     = nub $ f : extractFields e
extractFields (ExpressionCall r m v1 v2) = nub $ extractFieldR r ++ extractFieldV v1 ++ extractFieldV v2
extractFields (ExpressionValue v)        = extractFieldV v
extractFields (ExpressionNew _)          = []
extractFields (ExpressionIf e1 e2 e3)    = nub . concat $ map extractFields [e1, e2, e3]
extractFields (ExpressionLabel _ e)      = extractFields e
extractFields (ExpressionContinue _)     = []

extractFieldV :: Value -> [String]
extractFieldV (ValueBase _)      = []
extractFieldV (ValueReference r) = extractFieldR r

extractFieldR :: Reference -> [String]
extractFieldR (RefParameter _) = []
extractFieldR (RefField f)     = [f]


checkTCEn :: NDUsageState ()
checkTCEn = forAll' $ do
    s <- getState
    return [s]

checkTProg :: [Class] -> Either [String] ()
checkTProg cls = checkTProg' cls cls
    where checkTProg' :: [Class] -> [Class] -> Either [String] ()
          checkTProg' cls []     = Right ()
          checkTProg' cls (c:cs) = checkTClass cls c >> checkTProg' cls cs

checkTClass :: [Class] -> Class -> Either [String] ()
checkTClass cls c = do
    let name      = className c
    let usage     = currentUsage $ classUsage c
    let classdata = ClassData cls name
    let state     = UsageState (initFields (classFields c)) [] classdata
    let res       = runState (checkTUsage usage) [state]
    case res of
        (Right (_, term)) -> if any (\term' -> terminatedEnv (currentGamma term')) $ term
                                    then Right ()
                                    else Left $ ["could not typecheck: ", name, " " , show (length term)] ++ map (show . currentGamma) term
        (Left err)        -> Left [err]
                
terminatedEnv :: Gamma -> Bool
terminatedEnv = all (term . snd)

initFields :: [Field] -> Gamma
initFields [] = []
initFields ((Field (ClassFieldType cn) f):flds) = (f, BotType)  : initFields  flds
initFields ((Field (BaseFieldType t) f):flds)   = (f, (BaseType t))   : initFields  flds
