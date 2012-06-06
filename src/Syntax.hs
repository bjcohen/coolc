module Syntax where
          
newtype Symbol = Symbol String deriving (Eq,Ord)

instance Show Symbol where
  show (Symbol s) = s

class Symbolable a where
  newSymbol :: a -> Symbol

instance Symbolable String where
  newSymbol = Symbol
  
instance Symbolable Int where
  newSymbol i = Symbol $ show i
  
class SyntaxTerm a  
instance SyntaxTerm a => SyntaxTerm [a]
instance SyntaxTerm Program  
instance SyntaxTerm Class
instance SyntaxTerm Feature
instance SyntaxTerm Formal
instance SyntaxTerm Expression
instance SyntaxTerm Case
         
data Program =
  Program              { programClasses       :: Classes }
  deriving (Eq,Show)
                
type Classes = [Class]
data Class =
  Class                { className            :: Symbol
                       , classParent          :: Symbol
                       , classFeatures        :: Features
                       , classFilename        :: Symbol }
  deriving (Eq,Show)
                    
type Features = [Feature]
data Feature =
  Method               { methodName           :: Symbol
                       , methodFormals        :: Formals
                       , methodReturnType     :: Symbol
                       , methodExpr           :: Expression }
  | Attr               { attrName             :: Symbol
                       , attrType             :: Symbol
                       , attrInit             :: Expression }
  deriving (Eq,Show)

type Formals = [Formal]
data Formal =
  Formal               { formalName           :: Symbol
                       , formalType           :: Symbol }
  deriving (Eq,Show)

type Expressions = [Expression]
data Expression = Expression ExpressionBody Type deriving (Eq,Show)
data ExpressionBody =
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
  deriving (Eq,Show)

type Cases = [Case]
data Case =
  Branch               { branchName           :: Symbol
                       , branchType           :: Symbol
                       , branchExpr           :: Expression }
  deriving (Eq,Show)
           
data Type = NoType | Type Symbol deriving (Eq,Show)