-- TODO: error handling, pretty printing, classParent, classFilename

{
module Parser where
import qualified Data.Map as Map
import Control.Monad.State
import Lexer
import Syntax
}

%name cool
%tokentype { Token }
%error { parseError }
%monad { StateT Int StringTable }

%token
  "class"           { Token _ TCLASS _ }
  "else"            { Token _ TELSE _ }
  "fi"              { Token _ TFI _ }
  "if"              { Token _ TIF _ }
  "in"              { Token _ TIN _ }
  "inherits"        { Token _ TINHERITS _ }
  "isvoid"          { Token _ TISVOID _ }
  "let"             { Token _ TLET _ }
  "loop"            { Token _ TLOOP _ }
  "pool"            { Token _ TPOOL _ }
  "then"            { Token _ TTHEN _ }
  "while"           { Token _ TWHILE _ }
  "case"            { Token _ TCASE _ }
  "esac"            { Token _ TESAC _ }
  "new"             { Token _ TNEW _ }
  "of"              { Token _ TOF _ }
  "not"             { Token _ TNOT _ }
  str_const         { Token _ (TSTR_CONST $$) _ }
  int_const         { Token _ (TINT_CONST $$) _ }
  bool_const        { Token _ (TBOOL_CONST $$) _ }
  type_id           { Token _ (TTYPEID $$) _ }
  obj_id            { Token _ (TOBJECTID $$) _ }
  "<-"              { Token _ TASSIGN _ }
  "+"               { Token _ (TOPER OPLUS) _ }
  "-"               { Token _ (TOPER OMINUS) _ }
  "*"               { Token _ (TOPER OTIMES) _ }
  "/"               { Token _ (TOPER ODIV) _ }
  "<"               { Token _ (TOPER OLT) _ }
  "<="              { Token _ (TOPER OLE) _ }
  "="               { Token _ (TOPER OEQ) _ }
  "=>"              { Token _ TDARROW _ }
  "{"               { Token _ (TPUNC PLBRACE) _ }
  "}"               { Token _ (TPUNC PRBRACE) _ }
  "("               { Token _ (TPUNC PLPAREN) _ }
  ")"               { Token _ (TPUNC PRPAREN) _ }
  ";"               { Token _ (TPUNC PSEMI) _ }
  ":"               { Token _ (TPUNC PCOLON) _ }
  "~"               { Token _ (TPUNC PTILDE) _ }
  "."               { Token _ (TPUNC PPERIOD) _ }
  ","               { Token _ (TPUNC PCOMMA) _ }
  "@"               { Token _ (TPUNC PAT) _ }
  eof               { Token _ TEOF _ }                    
--lexerror          { Token _ (TERROR $$) _ }

%left "."
%left "@"
%left "~"
%left "isvoid"
%left "*" "/"
%left "+" "-"
%left "<=" "<" "="
%right LET
%left "not"
%right "<-"

%%

program :: { Program }
program :
    class ";" class_list                                       { Program{programClasses=$1:$3} }

class_list :: { Classes }
class_list :
    class ";" class_list                                       { $1:$3 }
  | eof                                                        { [] }

class :: { Class }
class :
    "class" type_id "{" feature_list "}"                       { Class{className=newSymbol $2,classParent=newSymbol "",classFeatures=$4,classFilename=newSymbol ""} }
  | "class" type_id "inherits" type_id "{" feature_list "}"    { Class{className=newSymbol $2,classParent=newSymbol $4,classFeatures=$6,classFilename=newSymbol ""} }

feature_list :: { Features }
feature_list :
    feature ";" feature_list                                   { $1:$3 }
  |                                                            { [] }

feature :: { Feature }
feature :  
    obj_id "(" formallist ")" ":" type_id "{" expr "}"         { Method{methodName=newSymbol $1,methodFormals=$3,methodReturnType=newSymbol $6,methodExpr=$8} }
  | obj_id ":" type_id                                         { Attr{attrName=newSymbol $1,attrType=newSymbol $3,attrInit=NoExpr} }
  | obj_id ":" type_id "<-" expr                               { Attr{attrName=newSymbol $1,attrType=newSymbol $3,attrInit=$5} }
    
formallist :: { Formals }                       
formallist :
    formal formallist_opt                                      { $1:$2 }
  |                                                            { [] }
  
formallist_opt :: { Formals }                                                               
formallist_opt :
    "," formal formallist_opt                                  { $2:$3 }
  |                                                            { [] }

formal :: { Formal }                                                               
formal :
  obj_id ":" type_id                                           { Formal{formalName=newSymbol $1,formalType=newSymbol $3} }

expr :: { Expression }
expr :
    obj_id "<-" expr                                           { Assign{assignName=newSymbol $1,assignExpr=$3} }
  | expr "." obj_id "(" arglist ")"                            { Dispatch{dispatchExpr=$1,dispatchName=newSymbol $3,dispatchActual=$5} }
  | expr "@" type_id "." obj_id "(" arglist ")"                { StaticDispatch{staticDispatchExpr=$1,staticDispatchType=newSymbol $3,staticDispatchName=newSymbol $5,staticDispatchActual=$7} }
  | obj_id "(" arglist ")"                                     { Dispatch{dispatchExpr=NoExpr,dispatchName=newSymbol $1,dispatchActual=$3} }
  | "if" expr "then" expr "else" expr "fi"                     { Cond{condPred=$2,condThen=$4,condExpression=$6} }
  | "while" expr "loop" expr "pool"                            { Loop{loopPred=$2,loopBody=$4} }
  | "{" expr ";" expr_seq "}"                                  { Block{blockBody=$2:$4} }
  | "let" obj_id ":" type_id optassign expr_let_items "in" expr %prec LET
                                                               { Let{letId=newSymbol $2,letType=newSymbol $4,letInit=$5,letBody=mkLetBody $6 $8} }
  | "case" expr "of" expr_case_list "esac"                     { TypCase{typCaseExpr=$2,typCaseCases=$4} }
  | "new" type_id                                              { New{newType=newSymbol $2} }
  | "isvoid" expr                                              { IsVoid{isVoidE=$2} }
  | expr "+" expr                                              { Plus{plusE1=$1,plusE2=$3} }
  | expr "-" expr                                              { Sub{subE1=$1,subE2=$3} }
  | expr "*" expr                                              { Mul{mulE1=$1,mulE2=$3} }
  | expr "/" expr                                              { Divide{divideE1=$1,divideE2=$3} }
  | "~" expr                                                   { Neg{negE=$2} }
  | expr "<" expr                                              { Lt{ltE1=$1,ltE2=$3} }
  | expr "<=" expr                                             { Le{leE1=$1,leE2=$3} }
  | expr "=" expr                                              { Eq{eqE1=$1,eqE2=$3} }
  | "not" expr                                                 { Comp{compE=$2} }
  | "(" expr ")"                                               { $2 }
  | obj_id                                                     { Object{objectName=newSymbol $1} }
  | int_const                                                  {% stAddAndReturn (\e -> IntConst{intConstToken=e}) $1 }
  | str_const                                                  {% stAddAndReturn (\e -> StringConst{stringConstToken=e}) $1 }
  | bool_const                                                 { BoolConst{boolConstVal=$1} }

expr_case_item :: { Case }
expr_case_item :
  obj_id ":" type_id "=>" expr ";"                             { Branch{branchName=newSymbol $1,branchType=newSymbol $3,branchExpr=$5} }

expr_case_list :: { Cases }
expr_case_list :
    expr_case_item expr_case_list                              { $1:$2 }
  |                                                            { [] }

expr_let_items :: { [(String, String, Expression)] }
expr_let_items :
    "," obj_id ":" type_id optassign expr_let_items            { ($2,$4,$5):$6 }
  |                                                            { [] }

optassign :: { Expression }
optassign :
    "<-" expr                                                  { $2 }
  |                                                            { NoExpr }

expr_seq :: { Expressions }
expr_seq :
    expr ";" expr_seq                                          { $1:$3 }
  |                                                            { [] }

arglist :: { Expressions }
arglist :
    expr arglist_opt                                           { $1:$2 }
  |                                                            { [] }

arglist_opt :: { Expressions }
arglist_opt :
    "," expr arglist_opt                                       { $2:$3 }
  |                                                            { [] }

{

data StringTable a = StringTable (Map.Map Symbol String) a
                   
instance Monad StringTable where
  (StringTable st a) >>= f =
    StringTable (Map.union st st') b
      where StringTable st' b = f a
  return k =
    StringTable Map.empty k

stAdd :: Symbol -> String -> StateT Int StringTable ()
stAdd sym str =
  StateT $ \i -> StringTable (Map.singleton sym str) ((), i)

stAddAndReturn :: (Symbol -> Expression) -> String -> StateT Int StringTable Expression
stAddAndReturn c str = 
  do n <- get
     put (n+1)
     sym <- return (newSymbol n)
     stAdd sym str
     return (c sym)
     
stGet :: StringTable a -> (Map.Map Symbol String, a)     
stGet (StringTable m k) = (m, k)

mkLetBody :: [(String, String, Expression)] -> Expression -> Expression     
mkLetBody [] ef = ef
mkLetBody ((id,typ,init):las) ef =
  Let{letId=newSymbol id,letType=newSymbol typ,letInit=init,letBody=mkLetBody las ef}

parseError :: [Token] -> a
parseError ts = error ("Parse error" ++ show ts)

}