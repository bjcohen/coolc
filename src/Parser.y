{
module Parser where
import Lexer
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

expression :
    class ";" expression                                       { [] }
  |                                                            { [] }
  
class :
    "class" type_id "{" class_opt "}"                          { [] }
  | "class" type_id "inherits" type_id "{" class_opt "}"       { [] }

class_opt :
    feature ";" class_opt                                      { [] }
  |                                                            { [] }
    
feature :  
    obj_id "(" formal feature_opt ")" ":" type_id "{" expr "}" { [] }
  | obj_id ":" type_id                                         { [] }
  | obj_id ":" type_id "<-" expr                               { [] }
    
feature_opt :
    "," formal feature_opt                                     { [] }
  |                                                            { [] }
  
formal :
  obj_id ":" type_id                                           { [] }

expr :
    obj_id "<-" expr                                           { [] }
  | expr "." obj_id "(" arglist ")"                            { [] }
  | expr "@" type_id "." obj_id "(" arglist ")"                { [] }
  | obj_id "(" arglist ")"                                     { [] }
  | "if" expr "then" expr "else" expr "fi"                     { [] }
  | "while" expr "loop" expr "pool"                            { [] }
  | "{" expr ";" expr_seq "}"                                  { [] }
  | "let" obj_id ":" type_id optassign expr_let "in" expr      { [] }
  | "case" expr "of" expr_case_plus "esac"                     { [] }
  | "new" type_id                                              { [] }
  | "isvoid" expr                                              { [] }
  | expr "+" expr                                              { [] }
  | expr "-" expr                                              { [] }
  | expr "*" expr                                              { [] }
  | expr "/" expr                                              { [] }
  | "~" expr                                                   { [] }
  | expr "<" expr                                              { [] }
  | expr "<=" expr                                             { [] }
  | expr "=" expr                                              { [] }
  | "not" expr                                                 { [] }
  | "(" expr ")"                                               { [] }
  | obj_id                                                     { [] }
  | int_const                                                  { [] }
  | str_const                                                  { [] }
  | bool_const                                                 { [] }

expr_case :
  obj_id ":" type_id "=>" ";"                                  { [] }

expr_case_plus :
    expr_case expr_case_plus                                   { [] }
  |                                                            { [] }

expr_let :
    "," obj_id ":" type_id optassign                           { [] }
  |                                                            { [] }

optassign :
    "<-" expr                                                  { [] }
  |                                                            { [] }

expr_seq :
    expr ";" expr_seq                                          { [] }
  |                                                            { [] }

arglist :
    expr arglist_opt                                           { [] }
  |                                                            { [] }

arglist_opt :
    "," arglist_opt                                            { [] }
  |                                                            { [] }

{

parseError :: [Token] -> a
parseError _ = error "Parse Error"

}