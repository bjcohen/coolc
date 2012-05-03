{
module Lexer where

import Numeric (readDec)
import Data.Maybe
import Control.Monad
}

%wrapper "monadUserState"

$white   = [\32\n\f\r\t\v]      -- whitespace
$digit   = 0-9			-- digits
$alpha   = [a-zA-Z]		-- alphabetic characters
$upper   = [A-Z]                -- upper case characters
$lower   = [a-z]                -- lower case characters
$punc    = [\{\}\(\)\;\:\~\.\,\@]
$alphnum = [$alpha $digit]
  
@tident  = $upper($alpha|_|$digit)*
@ident   = $lower($alpha|_|$digit)*
@oper    = "+" | "-" | "*" | "/" | "<" | "<=" | "="

tokens :-
  
<0>                  $white+		  { updatePos }		
<0>                  "--".*               { updatePos }
<0>                  "class"              { mkL CLASS }
<0>                  "else"               { mkL ELSE }
<0>                  "fi"                 { mkL FI }
<0>                  "if"                 { mkL IF }
<0>                  "in"                 { mkL IN }
<0>                  "inherits"           { mkL INHERITS }
<0>                  "isvoid"             { mkL ISVOID }
<0>                  "let"                { mkL LET }
<0>                  "loop"               { mkL LOOP }
<0>                  "pool"               { mkL POOL }
<0>                  "then"               { mkL THEN }
<0>                  "while"              { mkL WHILE }
<0>                  "case"               { mkL CASE }
<0>                  "esac"               { mkL ESAC }
<0>                  "new"                { mkL NEW }
<0>                  "of"                 { mkL OF }
<0>                  "not"                { mkL NOT }
<0>                  "*)"                 { lexerErrorAction "Unmatched *)"}
<0>                  \"                   { enterString `andBegin` state_string }
<state_string>       \n                   { leaveStringWithError "Unterminated string constant" `andBegin` state_initial }
<state_string>       \0                   { stringError "String contains null character." }
<state_string>       \"                   { leaveString `andBegin` state_initial }
<state_string>       \\$alphnum           { addControlToString }
<state_string>       .                    { addCurrentToString }                     
<0>                  $digit+              { getInteger }
<0>                  f[aA][lL][sS][eE]    { mkL (BOOL_CONST False) }
<0>                  t[rR][uU][eE]        { mkL (BOOL_CONST True) }
<0>                  @tident              { getIdent TYPEID }
<0>                  @ident               { getIdent OBJECTID }
<0>                  "<-"                 { mkL ASSIGN }
<0>                  "=>"                 { mkL DARROW }
<0>                  @oper                { getOper }
<0>                  $punc                { getPunc }
<0>                  "(*"                 { embedComment `andBegin` state_comment }                      
<state_comment>      "(*"                 { embedComment }
<state_comment>      "*)"                 { unembedComment }
<state_comment>      .                    { updatePos }
<state_comment>      \n                   { updatePos }
<0>                  .                    { lexerIllegalCharacter }
                      
{
                      
data Token = Token AlexPosn TokenClass (Maybe String)
instance Show Token where
  show (Token _ EOF _) = ""
  show (Token p cl mbs) = showp p ++ " " ++ show cl
    where
      showp (AlexPn _ ln _) = "#" ++ show ln
      showmbs Nothing = ""
      showmbs (Just s) = show s

instance Show TokenClass where
  show CLASS = "CLASS"
  show ELSE = "ELSE"
  show FI = "FI"
  show IF = "IF"
  show IN = "IN"
  show INHERITS = "INHERITS"
  show ISVOID = "ISVOID"
  show LET = "LET"
  show LOOP = "LOOP"
  show POOL = "POOL"
  show THEN = "THEN"
  show WHILE = "WHILE"
  show CASE = "CASE"
  show ESAC = "ESAC"
  show NEW = "NEW"
  show OF = "OF"
  show NOT = "NOT"
  show (STR_CONST s) = "STR_CONST " ++ show s
  show (INT_CONST i) = "INT_CONST " ++ show i
  show (BOOL_CONST b) = "BOOL_CONST " ++
                        (if b then "true" else "false")
  show (TYPEID s) = "TYPEID " ++ s
  show (OBJECTID s) = "OBJECTID " ++ s
  show ASSIGN = "ASSIGN"
  show (OPER s) = show (head s)
  show LE = "LE"
  show DARROW = "DARROW"
  show (PUNC s) = show (head s)
  show (ERROR s) = "ERROR " ++ show s
  show EOF = ""

data Oper = PLUS | MINUS | TIMES | DIV | LT | LTE | EQ
    
data Punc = LBRACE | RBRACE | LPAREN | RPAREN | SEMI | COLON | TILDE

-- token type

data TokenClass =
  CLASS
  | ELSE
  | FI
  | IF
  | IN
  | INHERITS
  | ISVOID
  | LET
  | LOOP
  | POOL
  | THEN
  | WHILE
  | CASE
  | ESAC
  | NEW
  | OF
  | NOT
  | STR_CONST String
  | INT_CONST Int
  | BOOL_CONST Bool
  | TYPEID String
  | OBJECTID String
  | ASSIGN
  | OPER String
  | LE
  | DARROW
  | PUNC String
  | ERROR String
  | EOF
  | LET_STMT -- PA3?
  deriving (Eq)

mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, str) len = return (Token p c (Just (take len str)))

-- state aliases

state_initial :: Int
state_initial = 0

-- UserState definition

data StringState = In | Out | Error String deriving Eq

data AlexUserState = AlexUserState
                     {
                       lexerCommentDepth :: Int
                     , lexerStringValue  :: String
                     , lexerStringState  :: StringState
                     , lexerCurrentPos   :: AlexPosn
                     }
                     
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                    {
                      lexerCommentDepth = 0
                    , lexerStringValue = ""
                    , lexerStringState = Out
                    , lexerCurrentPos = undefined
                    }
  
getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

getLexerStringState :: Alex StringState
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: StringState -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

getLexerCurrentPos :: Alex AlexPosn
getLexerCurrentPos = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCurrentPos ust)

setLexerCurrentPos :: AlexPosn -> Alex ()
setLexerCurrentPos p = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCurrentPos=p}}, ())

-- actions 
type Action = AlexInput -> Int -> Alex Token
enterString, addCurrentToString, addControlToString, leaveString :: Action
getInteger, getOper, getPunc :: Action
getIdent :: (String -> TokenClass) -> Action
embedComment, unembedComment :: Action
lexerError :: String -> AlexPosn -> Maybe String -> Token
lexerErrorAction, leaveStringWithError, stringError :: String -> Action
lexerIllegalCharacter :: Action
skip, updatePos :: Action

updatePos (p, _, _) _ =
  do setLexerCurrentPos p
     alexMonadScan

embedComment input len = 
  do cd <- getLexerCommentDepth
     setLexerCommentDepth (cd + 1)
     skip input len
     
unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len
       
enterString input len =
    do setLexerStringState In
       setLexerStringValue ""
       skip input len 
       
addCurrentToString (p, _, input) len =
  do addCharToLexerStringValue c
     alexMonadScan
       where
         c = if (len == 1) then head input else error "Invalid call to addCurrentToString"

addControlToString (p, _, input) len =
  do addCharToLexerStringValue c
     alexMonadScan
       where
         cc = if (len == 2) then take len input else error "Control code not length 2"
         s = head (tail cc)
         c = case s of
           'b' -> '\b'
           't' -> '\t'
           'n' -> '\n'
           'f' -> '\f'
           _ -> s

leaveString ai@(p, _, input) len =
    do ss <- getLexerStringState
       case ss of
         Error msg -> lexerErrorAction msg ai len
         Out -> error "leaveString while out of string"
         In -> do s <- getLexerStringValue
                  setLexerStringState Out
                  if (length s <= 1024)
                    then return (Token p (STR_CONST (reverse s)) (Just t))
                    else return (Token p (ERROR "String constant too long") (Just t))
                      where t = take len input

leaveStringWithError msg ai len =          
  do ss <- getLexerStringState
     case ss of
       In -> do setLexerStringState (Error msg)
                leaveString ai len
       Out -> error "leaveStringWithError while out of string"
       Error _ -> leaveString ai len
     
stringError msg ai@(p, _, input) len =
  do setLexerStringState (Error msg)
     skip ai len
       
getInteger ai@(p, _, input) len = 
  if (length r == 1) then return (Token p (INT_CONST (fst (head r))) (Just s)) else return (lexerError "Invalid Int" p (Just s))
    where
      s = take len input
      r = readDec s
  
getIdent tc (p, _, input) len =
  return (Token p (tc s) (Just s))
    where
      s = take len input
  
getOper ai@(p, _, input) len =
  if len == 1 then getIdent OPER ai len
  else return (Token p LE (Just s))
       where
         s = take len input
  
getPunc = getIdent PUNC    
  
lexerErrorAction msg (p, _, input) len =
  return (lexerError msg p (Just (take len input)))
          
lexerError msg p ms = 
  (Token p (ERROR msg) ms)
      
lexerIllegalCharacter (p, _, input) len =
  return (lexerError c p (Just c))
    where
      c = take len input
  
alexEOF :: Alex Token
alexEOF = 
  do
    p <- getLexerCurrentPos
    return (Token p EOF Nothing)
  
scanner :: String -> Either String [Token]
scanner str = let loop = do t <- alexMonadScan
                            let tok@(Token p cl s) = t
                            if (cl == EOF)
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((f1 == Out) && (d2 == 0))
                                         then return [tok]
                                         else case f1 of
                                         In -> return [lexerError "EOF in string constant" p s]
                                         Out -> return [lexerError "EOF in comment" p s]
                                         Error msg -> return [lexerError msg p s]
                                    
                               else do setLexerCurrentPos p -- hack because alex doesn't return the position with EOF
                                       toks <- loop
                                       return (tok : toks)
              in  runAlex str loop

-- lexer :: Alex Token
-- lexer =
--     do
--        inp <- alexGetInput
--        sc <- alexGetStartCode
--        case alexScan inp sc of
--             AlexEOF              -> alexEOF sc
--             AlexError _          -> alexError "lexical error"
--             AlexSkip  inp1 _     -> do
--                                        alexSetInput inp1
--                                        lexer
--             AlexToken inp1 len t -> do
--                                        alexSetInput inp1
--                                        t inp len

}