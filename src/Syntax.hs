module Syntax where

newtype Symbol = Symbol String deriving (Eq)

data Program =
  Program              { programClasses       :: Classes }
  deriving Eq
                
type Classes = [Class]
data Class =
  Class                { className            :: Symbol
                       , classParent          :: Symbol
                       , classFeatures        :: Features
                       , classFilename        :: Symbol }
  deriving Eq
                    
type Features = [Feature]
data Feature =
  Method               { methodName           :: Symbol
                       , methodFormals        :: Formals
                       , methodReturnType     :: Symbol
                       , methodExpr           :: Expression }
  | Attr               { attrName             :: Symbol
                       , attrType             :: Symbol
                       , attrInit             :: Expression }
  deriving Eq

type Formals = [Formal]
data Formal =
  Formal               { formalName           :: Symbol
                       , formalType           :: Symbol }
  deriving Eq

type Expressions = [Expression]
data Expression =
  Assign               { assignName           :: Symbol
                       , assignExpr           :: Expression }
  | StaticDispatch     { staticDispatchExpr   :: Expression
                       , staticDispatchType   :: Symbol
                       , staticDispatchName   :: Symbol
                       , staticDispatchActual :: Expressions }
  | Dispatch           { dispatchExpr         :: Expression
                       , dispatchName         :: Symbol
                       , dispatchActual       :: Expressions }
  | Cond               { condPred             :: Expression
                       , condThen             :: Expression
                       , condExpression       :: Expression }
  | Loop               { loopPred             :: Expression
                       , loopBody             :: Expression }
  | TypCase            { typCaseExpr          :: Expression
                       , typCaseCases         :: Cases }
  | Block              { blockBody            :: Expressions }
  | Let                { letId                :: Symbol
                       , letType              :: Symbol
                       , letInit              :: Expression
                       , letBody              :: Expression }
  | Plus               { plusE1               :: Expression
                       , plusE2               :: Expression }
  | Sub                { subE1                :: Expression
                       , subE2                :: Expression }
  | Mul                { mulE1                :: Expression
                       , mulE2                :: Expression }
  | Divide             { divideE1             :: Expression
                       , divideE2             :: Expression }
  | Neg                { negE                 :: Expression }
  | Lt                 { ltE1                 :: Expression
                       , ltE2                 :: Expression }
  | Eq                 { eqE1                 :: Expression
                       , eqE2                 :: Expression }
  | Le                 { leE1                 :: Expression
                       , leE2                 :: Expression }
  | Comp               { compE                :: Expression }
  | IntConst           { intConstToken        :: Symbol }
  | BoolConst          { boolConstVal         :: Bool }
  | StringConst        { stringConstToken     :: Symbol }
  | New                { newType              :: Symbol }
  | IsVoid             { isVoidE              :: Expression }
  | NoExpr
  | Object             { objectName           :: Symbol }
  deriving Eq

type Cases = [Case]
data Case =
  Branch               { branchName           :: Symbol
                       , branchType           :: Symbol
                       , branchExpr           :: Expression }
  deriving Eq