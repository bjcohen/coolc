{
module Parser where
import qualified Data.Map as Map
import Control.Monad.State
import Lexer
import Syntax
import SCUtil
import Type
}

%name cool
%tokentype { Token }
%error { parseError }
%monad { ParseState }
%lexer { lexwrap } { Token _ TEOF _ }

%token
                    -- error is 0
  "class"           { Token _ TCLASS _ }
  "else"            { Token _ TELSE _ }
  "fi"              { Token _ TFI _ }
  "if"              { Token _ TIF _ }
  "in"              { Token _ TIN _ } -- 5
  "inherits"        { Token _ TINHERITS _ }
  "isvoid"          { Token _ TISVOID _ }
  "let"             { Token _ TLET _ }
  "loop"            { Token _ TLOOP _ }
  "pool"            { Token _ TPOOL _ } -- 10
  "then"            { Token _ TTHEN _ }
  "while"           { Token _ TWHILE _ }
  "case"            { Token _ TCASE _ }
  "esac"            { Token _ TESAC _ }
  "new"             { Token _ TNEW _ } -- 15
  "of"              { Token _ TOF _ }
  "not"             { Token _ TNOT _ }
  str_const         { Token _ (TSTR_CONST $$) _ }
  int_const         { Token _ (TINT_CONST $$) _ }
  bool_const        { Token _ (TBOOL_CONST $$) _ } -- 20
  type_id           { Token _ (TTYPEID $$) _ }
  obj_id            { Token _ (TOBJECTID $$) _ }
  "<-"              { Token _ TASSIGN _ }
  "+"               { Token _ (TOPER OPLUS) _ }
  "-"               { Token _ (TOPER OMINUS) _ } -- 25
  "*"               { Token _ (TOPER OTIMES) _ }
  "/"               { Token _ (TOPER ODIV) _ }
  "<"               { Token _ (TOPER OLT) _ }
  "<="              { Token _ (TOPER OLE) _ }
  "="               { Token _ (TOPER OEQ) _ } -- 30
  "=>"              { Token _ TDARROW _ }
  "{"               { Token _ (TPUNC PLBRACE) _ }
  "}"               { Token _ (TPUNC PRBRACE) _ }
  "("               { Token _ (TPUNC PLPAREN) _ }
  ")"               { Token _ (TPUNC PRPAREN) _ } -- 35
  ";"               { Token _ (TPUNC PSEMI) _ }
  ":"               { Token _ (TPUNC PCOLON) _ }
  "~"               { Token _ (TPUNC PTILDE) _ }
  "."               { Token _ (TPUNC PPERIOD) _ }
  ","               { Token _ (TPUNC PCOMMA) _ } -- 40
  "@"               { Token _ (TPUNC PAT) _ }
                    -- %eof is 42
--lexerror          { Token _ (TERROR $$) _ }

%right "<-"
%left "not"
%right LET
%left "<=" "<" "="
%left "+" "-"
%left "*" "/"
%left "isvoid"
%left "~"
%left "@"
%left "."

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
    "class" type_id "{" feature_list "}"                       {% stMkWithFilename (\fn -> Class{className=newSymbol $2,classParent=newSymbol "Object",classFeatures=$4,classFilename=newSymbol fn}) }
  | "class" type_id "inherits" type_id "{" feature_list "}"    {% stMkWithFilename (\fn -> Class{className=newSymbol $2,classParent=newSymbol $4,classFeatures=$6,classFilename=newSymbol fn}) }

feature_list :: { Features }
feature_list :
    feature ";" feature_list                                   { $1:$3 }
  |                                                            { [] }

feature :: { Feature }
feature :  
    obj_id "(" formallist ")" ":" type_id "{" expr "}"         { Method{methodName=newSymbol $1,methodFormals=$3,methodReturnType=newSymbol $6,methodExpr=$8} }
  | obj_id ":" type_id                                         { Attr{attrName=newSymbol $1,attrType=newSymbol $3,attrInit=mkE NoExpr} }
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
    obj_id ":" type_id                                         { Formal{formalName=newSymbol $1,formalType=newSymbol $3} }

expr :: { Expression }
expr :
    obj_id "<-" expr                                           { mkE Assign{assignName=newSymbol $1,assignExpr=$3} }
  | expr "." obj_id "(" arglist ")"                            { mkE Dispatch{dispatchExpr=$1,dispatchName=newSymbol $3,dispatchActual=$5} }
  | expr "@" type_id "." obj_id "(" arglist ")"                { mkE StaticDispatch{staticDispatchExpr=$1,staticDispatchType=newSymbol $3,staticDispatchName=newSymbol $5,staticDispatchActual=$7} }
  | obj_id "(" arglist ")"                                     { mkE Dispatch{dispatchExpr=mkE Object{objectName=newSymbol "self"},dispatchName=newSymbol $1,dispatchActual=$3} }
  | "if" expr "then" expr "else" expr "fi"                     { mkE Cond{condPred=$2,condThen=$4,condExpression=$6} }
  | "while" expr "loop" expr "pool"                            { mkE Loop{loopPred=$2,loopBody=$4} }
  | "{" expr_plus "}"                                          { mkE Block{blockBody=$2} }
  | "let" obj_id ":" type_id optassign expr_let_items "in" expr %prec LET
                                                               { mkE Let{letId=newSymbol $2,letType=newSymbol $4,letInit=$5,letBody=mkLetBody $6 $8} }
  | "case" expr "of" expr_case_list "esac"                     { mkE TypCase{typCaseExpr=$2,typCaseCases=$4} }
  | "new" type_id                                              { mkE New{newType=newSymbol $2} }
  | "isvoid" expr                                              { mkE IsVoid{isVoidE=$2} }
  | expr "+" expr                                              { mkE Plus{plusE1=$1,plusE2=$3} }
  | expr "-" expr                                              { mkE Sub{subE1=$1,subE2=$3} }
  | expr "*" expr                                              { mkE Mul{mulE1=$1,mulE2=$3} }
  | expr "/" expr                                              { mkE Divide{divideE1=$1,divideE2=$3} }
  | "~" expr                                                   { mkE Neg{negE=$2} }
  | expr "<" expr                                              { mkE Lt{ltE1=$1,ltE2=$3} }
  | expr "<=" expr                                             { mkE Le{leE1=$1,leE2=$3} }
  | expr "=" expr                                              { mkE Eq{eqE1=$1,eqE2=$3} }
  | "not" expr                                                 { mkE Comp{compE=$2} }
  | "(" expr ")"                                               { $2 }
  | obj_id                                                     { mkE Object{objectName=newSymbol $1} }
  | int_const                                                  {% stAddAndReturn (\e -> mkE IntConst{intConstToken=e}) $1 }
  | str_const                                                  {% stAddAndReturn (\e -> mkE StringConst{stringConstToken=e}) $1 }
  | bool_const                                                 { mkE BoolConst{boolConstVal=$1} }

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
  |                                                            { mkE NoExpr }

expr_plus :: { Expressions }
expr_plus :
    expr ";" expr_seq                                         { $1:$3 }
  |                                                           { [] }

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

-- Symbol ID, Filename, Tokens to Process, Line Number
type ParseState a = StateT (Int, String, [Token], Int) StringTable a
  
stAdd :: Symbol -> String -> ParseState ()
stAdd sym tr =
  StateT $ \i -> (tAdd sym tr) ((), i)

stAddAndReturn :: (Symbol -> Expression) -> String -> ParseState Expression
stAddAndReturn c tr = 
  do (n, s, tl, ln) <- get
     put (n+1, s, tl, ln)
     sym <- return (newSymbol n)
     stAdd sym tr
     return (c sym)
     
stMkWithFilename :: SyntaxTerm a => (String -> a) -> ParseState a
stMkWithFilename c =
  do (_, filename, _, _) <- get
     return (c filename)
     
stGet :: StringTable a -> (Map.Map Symbol String, a)     
stGet st = (tGetT st, tGetV st)
                    
mkLetBody :: [(String, String, Expression)] -> Expression -> Expression     
mkLetBody [] ef = ef
mkLetBody ((id,typ,init):las) ef =
  mkE Let{letId=newSymbol id,letType=newSymbol typ,letInit=init,letBody=mkLetBody las ef}

mkE :: ExpressionBody -> Expression
mkE e = Expression e NoType

newtype ParseError = ParseError String
instance SyntaxTerm ParseError

parseError :: Token -> ParseState a
parseError tok = do (n, s, ts, ln) <- get
                    error (show s ++ ", line " ++ show ln ++ ": syntax error at or near " ++ show tok)

lexwrap :: (Token -> ParseState a) -> ParseState a
lexwrap cont = lexer >>= (\tok -> cont tok)

lexer :: ParseState Token
lexer = do (n, s, t:ts, _) <- get
           let Token (AlexPn _ ln _) _ _ = t
           put (n, s, ts, ln)
           return t

}
