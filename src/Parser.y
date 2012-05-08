-- TODO: shift/reduce conflicts, let

{
module Parser where
import Lexer
import Syntax
}

%name cool
%tokentype { Token }
%error { parseError }

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
--lexerror          { Token _ (TERROR $$) _ }

%left "."
%left "@"
%left "~"
%left "isvoid"
%left "*" "/"
%left "+" "-"
%left "<=" "<" "="
%left "not"
%right "<-"

%%

program :: { Program }
program :
    class ";" class_list                                       { Program{programClasses=$1:$3} }

class_list :: { Classes }
class_list :
    class ";" class_list                                       { $1:$3 }
  |                                                            { [] }

class :: { Class }
class :
    "class" type_id "{" feature_list "}"                       { Class{className=Symbol $2,classParent=Symbol "",classFeatures=$4,classFilename=Symbol ""} }
  | "class" type_id "inherits" type_id "{" feature_list "}"    { Class{className=Symbol $2,classParent=Symbol $4,classFeatures=$6,classFilename=Symbol ""} }

feature_list :: { Features }
feature_list :
    feature ";" feature_list                                   { $1:$3 }
  |                                                            { [] }

feature :: { Feature }
feature :  
    obj_id "(" formal formal_list ")" ":" type_id "{" expr "}" { Method{methodName=Symbol $1,methodFormals=$3:$4,methodReturnType=Symbol $7,methodExpr=$9} }
  | obj_id ":" type_id                                         { Attr{attrName=Symbol $1,attrType=Symbol $3,attrInit=NoExpr} }
  | obj_id ":" type_id "<-" expr                               { Attr{attrName=Symbol $1,attrType=Symbol $3,attrInit=$5} }
    
formal_list :: { Formals }                       
formal_list :
    "," formal formal_list                                     { $2:$3 }
  |                                                            { [] }
  
formal :: { Formal }                                                               
formal :
  obj_id ":" type_id                                           { Formal{formalName=Symbol $1,formalType=Symbol $3} }

expr :: { Expression }
expr :
    obj_id "<-" expr                                           { Assign{assignName=Symbol $1,assignExpr=$3} }
  | expr "." obj_id "(" arglist ")"                            { Dispatch{dispatchExpr=$1,dispatchName=Symbol $3,dispatchActual=$5} }
  | expr "@" type_id "." obj_id "(" arglist ")"                { StaticDispatch{staticDispatchExpr=$1,staticDispatchType=Symbol $3,staticDispatchName=Symbol $5,staticDispatchActual=$7} }
  | obj_id "(" arglist ")"                                     { Dispatch{dispatchExpr=NoExpr,dispatchName=Symbol $1,dispatchActual=$3} }
  | "if" expr "then" expr "else" expr "fi"                     { Cond{condPred=$2,condThen=$4,condExpression=$6} }
  | "while" expr "loop" expr "pool"                            { Loop{loopPred=$2,loopBody=$4} }
  | "{" expr ";" expr_seq "}"                                  { Block{blockBody=$2:$4} }
  | "let" obj_id ":" type_id optassign expr_let_items "in" expr{ error "TODO" }
  | "case" expr "of" expr_case_list "esac"                     { TypCase{typCaseExpr=$2,typCaseCases=$4} }
  | "new" type_id                                              { New{newType=Symbol $2} }
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
  | obj_id                                                     { Object{objectName=Symbol $1} }
  | int_const                                                  { error "TODO" }
  | str_const                                                  { error "TODO" }
  | bool_const                                                 { BoolConst{boolConstVal=$1} }

expr_case_item :: { Case }
expr_case_item :
  obj_id ":" type_id "=>" expr ";"                             { Branch{branchName=Symbol $1,branchType=Symbol $3,branchExpr=$5} }

expr_case_list :: { Cases }
expr_case_list :
    expr_case_item expr_case_list                              { $1:$2 }
  |                                                            { [] }

expr_let_items :: { Expression }
expr_let_items :
    "," obj_id ":" type_id optassign                           { error "TODO" }
  |                                                            { error "TODO" }

optassign :: { Maybe Expression }
optassign :
    "<-" expr                                                  { Just $2 }
  |                                                            { Nothing }

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

parseError :: [Token] -> a
parseError _ = error "Parse Error"

}