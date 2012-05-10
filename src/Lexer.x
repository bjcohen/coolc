{
module Lexer where

import Numeric (readDec, showOct)
import Data.Maybe
import Control.Monad
import Data.Char (isPrint, ord)
import Text.Printf (printf)
}

%wrapper "monadUserState"

$white   = [\32\n\f\r\t\v]      -- whitespace
$digit   = 0-9			-- digits
$alpha   = [a-zA-Z]		-- alphabetic characters
$upper   = [A-Z]                -- upper case characters
$lower   = [a-z]                -- lower case characters
$punc    = [\{\}\(\)\;\:\~\.\,\@]
$alphnum = [$alpha $digit]
$unprint = [\n \b \f \r \t \\ \" \-]
$a       = [aA]
$b       = [bB]
$c       = [cC]
$d       = [dD]
$e       = [eE]
$f       = [fF]
$g       = [gG]
$h       = [hH]
$i       = [iI]
$j       = [jJ]
$k       = [kK]
$l       = [lL]
$m       = [mM]
$n       = [nN]
$o       = [oO]
$p       = [pP]
$q       = [qQ]
$r       = [rR]
$s       = [sS]
$t       = [tT]
$u       = [uU]
$v       = [vV]
$w       = [wW]
$x       = [xX]
$y       = [yY]
$z       = [zZ]

@tident  = $upper($alpha|_|$digit)*
@ident   = $lower($alpha|_|$digit)*
@oper    = "+" | "-" | "*" | "/" | "<" | "<=" | "="

tokens :-
  
<0>                  $white+	          { updatePos }		
<0>                  "--".*               { updatePos }
<0>                  $c$l$a$s$s           { mkL TCLASS }
<0>                  $e$l$s$e             { mkL TELSE }
<0>                  $f$i                 { mkL TFI }
<0>                  $i$f                 { mkL TIF }
<0>                  $i$n                 { mkL TIN }
<0>                  $i$n$h$e$r$i$t$s     { mkL TINHERITS }
<0>                  $i$s$v$o$i$d         { mkL TISVOID }
<0>                  $l$e$t               { mkL TLET }
<0>                  $l$o$o$p             { mkL TLOOP }
<0>                  $p$o$o$l             { mkL TPOOL }
<0>                  $t$h$e$n             { mkL TTHEN }
<0>                  $w$h$i$l$e           { mkL TWHILE }
<0>                  $c$a$s$e             { mkL TCASE }
<0>                  $e$s$a$c             { mkL TESAC }
<0>                  $n$e$w               { mkL TNEW }
<0>                  $o$f                 { mkL TOF }
<0>                  $n$o$t               { mkL TNOT }
<0>                  "*)"                 { lexerErrorAction "Unmatched *)"}
<0>                  \"                   { enterString `andBegin` state_string }
<state_string>       \n                   { leaveStringWithError "Unterminated string constant" `andBegin` state_initial }
<state_string>       \0                   { stringError "String contains null character." }
<state_string>       \\$alphnum           { addControlToString }
<state_string>       \\$unprint           { addControlToString }
<state_string>       \"                   { leaveString `andBegin` state_initial }
<state_string>       .                    { addCurrentToString }                     
<0>                  $digit+              { getInteger }
<0>                  f$a$l$s$e            { mkL (TBOOL_CONST False) }
<0>                  t$r$u$e              { mkL (TBOOL_CONST True) }
<0>                  @tident              { getIdent TTYPEID }
<0>                  @ident               { getIdent TOBJECTID }
<0>                  "<-"                 { mkL TASSIGN }
<0>                  "=>"                 { mkL TDARROW }
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
  show (Token _ TEOF _) = "#EOF"
  show (Token p cl mbs) = showp p ++ " " ++ show cl
    where
      showp (AlexPn _ ln _) = "#" ++ show ln
      showmbs Nothing = ""
      showmbs (Just s) = show s

scshow :: String -> String
scshow s = "\"" ++ foldr (++) "" (map show' s) ++ "\""
  where show' c = if and [isPrint c, not (c == '\\'), not (c == '"')] then [c]
                  else case c of
                    '\b' -> "\\b"
                    '\t' -> "\\t"
                    '\n' -> "\\n"
                    '\f' -> "\\f"
                    '\\' -> "\\\\"
                    '"' -> "\\\""
                    _    -> "\\" ++ printf "%.3o" (ord c)

instance Show TokenClass where
  show TCLASS = "CLASS"
  show TELSE = "ELSE"
  show TFI = "FI"
  show TIF = "IF"
  show TIN = "IN"
  show TINHERITS = "INHERITS"
  show TISVOID = "ISVOID"
  show TLET = "LET"
  show TLOOP = "LOOP"
  show TPOOL = "POOL"
  show TTHEN = "THEN"
  show TWHILE = "WHILE"
  show TCASE = "CASE"
  show TESAC = "ESAC"
  show TNEW = "NEW"
  show TOF = "OF"
  show TNOT = "NOT"
  show (TSTR_CONST s) = "STR_CONST " ++ scshow s
  show (TINT_CONST s) = "INT_CONST " ++ s
  show (TBOOL_CONST b) = "BOOL_CONST " ++
                         (if b then "true" else "false")
  show (TTYPEID s) = "TYPEID " ++ s
  show (TOBJECTID s) = "OBJECTID " ++ s
  show TASSIGN = "ASSIGN"
  show (TOPER o) = show o
  show TDARROW = "DARROW"
  show (TPUNC o) = show o
  show (TERROR s) = "ERROR " ++ show s
  show TEOF = ""
              
instance Show Oper where
  show OPLUS   = "'+'"
  show OMINUS  = "'-'"
  show OTIMES  = "'*'"
  show ODIV    = "'/'"
  show OLT     = "'<'"
  show OLE     = "LE"
  show OEQ     = "'='"
                  
instance Show Punc where
  show PLBRACE = "'{'"
  show PRBRACE = "'}'"
  show PLPAREN = "'('"
  show PRPAREN = "')'"
  show PSEMI   = "';'"
  show PCOLON  = "':'"
  show PTILDE  = "'~'"
  show PPERIOD = "'.'"
  show PCOMMA  = "','"
  show PAT     = "'@'"

data Oper = OPLUS | OMINUS | OTIMES | ODIV | OLT | OLE | OEQ deriving Eq
    
data Punc = PLBRACE | PRBRACE | PLPAREN | PRPAREN | PSEMI |
            PCOLON | PTILDE | PPERIOD | PCOMMA | PAT deriving Eq

-- token type

data TokenClass =
  TCLASS
  | TELSE
  | TFI
  | TIF
  | TIN
  | TINHERITS
  | TISVOID
  | TLET
  | TLOOP
  | TPOOL
  | TTHEN
  | TWHILE
  | TCASE
  | TESAC
  | TNEW
  | TOF
  | TNOT
  | TSTR_CONST String
  | TINT_CONST String
  | TBOOL_CONST Bool
  | TTYPEID String
  | TOBJECTID String
  | TASSIGN
  | TOPER Oper
  | TDARROW
  | TPUNC Punc
  | TERROR String
  | TEOF
  | TLET_STMT -- PA3?
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
                    , lexerCurrentPos = AlexPn 0 0 0
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
     updatePos input len
     
unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       updatePos input len
       
enterString input len =
    do setLexerStringState In
       setLexerStringValue ""
       updatePos input len 
       
addCurrentToString ai@(p, _, input) len =
  do addCharToLexerStringValue c
     updatePos ai len
       where
         c = if (len == 1) then head input else error "Invalid call to addCurrentToString"

addControlToString ai@(p, _, input) len =
  do addCharToLexerStringValue c
     updatePos ai len
       where
         cc = if (len == 2) then take len input else error "Control code not length 2"
         s = head (tail cc)
         c = case s of
           'b' -> '\b'
           't' -> '\t'
           'n' -> '\n'
           'f' -> '\f'
           _   -> s

leaveString ai@(p, _, input) len =
    do ss <- getLexerStringState
       case ss of
         Error msg -> do setLexerStringState Out
                         lexerErrorAction msg ai len
         Out -> error "leaveString while out of string"
         In -> do s <- getLexerStringValue
                  setLexerStringState Out
                  if (length s <= 1024)
                    then return (Token p (TSTR_CONST (reverse s)) (Just t))
                    else lexerErrorAction "String constant too long" ai len
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
     updatePos ai len
       
getInteger ai@(p, _, input) len = 
  return (Token p (TINT_CONST s) (Just s))
    where
      s = take len input
  
getIdent tc (p, _, input) len =
  return (Token p (tc s) (Just s))
    where
      s = take len input
  
getOper ai@(p, _, input) len =
  return (Token p (TOPER o) (Just s))
    where
      s = take len input
      o = case s of
        "+"  -> OPLUS
        "-"  -> OMINUS
        "*"  -> OTIMES
        "/"  -> ODIV
        "<"  -> OLT
        "<=" -> OLE
        "="  -> OEQ
        _    -> error "Got bad operator in lexer"
  
getPunc ai@(p, _, input) len =
  return (Token p (TPUNC o) (Just s))
    where
      s = take len input
      o = case s of
        "{" -> PLBRACE
        "}" -> PRBRACE
        "(" -> PLPAREN
        ")" -> PRPAREN
        ";" -> PSEMI
        ":" -> PCOLON
        "~" -> PTILDE
        "." -> PPERIOD
        "," -> PCOMMA
        "@" -> PAT
        _   -> error "Got bad punctuation in lexer"
  
lexerErrorAction msg (p, _, input) len =
  return (lexerError msg p (Just (take len input)))
          
lexerError msg p ms = 
  (Token p (TERROR msg) ms)
      
lexerIllegalCharacter (p, _, input) len =
  return (lexerError c p (Just c))
    where
      c = take len input
  
alexEOF :: Alex Token
alexEOF = 
  do
    p <- getLexerCurrentPos
    return (Token p TEOF Nothing)
  
scanner :: String -> Either String [Token]
scanner str = let loop = do t <- alexMonadScan
                            let tok@(Token p cl s) = t
                            if (cl == TEOF)
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