module Parser.ParsecParser (program, runCoolParser) where

import Text.Parsec.Expr
import Text.Parsec.Token (GenLanguageDef(..), lexeme)
import Text.Parsec
import qualified Text.Parsec.Token as T

import qualified Data.Map as Map

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Supply

import Syntax
import SCUtil
import Type
import Parser.PrettyPrint (pp)

type ST = Map.Map Symbol String
type ParseMonad = WriterT ST (Supply Int)

instance MonadSupply s m => MonadSupply s (ParsecT s' u m) where
  supply = lift supply
  
instance MonadWriter w m => MonadWriter w (ParsecT s u m) where
  tell = lift . tell
  --TODO: finish this instance declaration

addString :: String -> ParseMonad Symbol
addString val =
  do name <- supply
     let sym = newSymbol name
     tell $ Map.singleton sym val
     return sym
  
addStringP :: String -> CoolParser Symbol
addStringP val =
  do name <- supply
     let sym = newSymbol name
     tell $ Map.singleton sym val
     return $ sym
              
-- type ParseMonad = Supply Int

runCoolParser :: CoolParser a -> String -> SourceName -> (Either ParseError a, ST)
runCoolParser p input fname =
  evalSupply (runWriterT $ runParserT p fname fname input) [0..]
  
type UserData = String

type CoolParser = ParsecT String UserData ParseMonad

program :: CoolParser Program
program = do
  whiteSpace
  cs <- endBy1 cClass semi
  eof
  return $ Program{programClasses=cs}
  <?> "program"

cClass :: CoolParser Class
cClass = do
  reserved "class"
  name <- type_id
  pname <- option "Object" (do { reserved "inherits"; type_id })    
  fs <- braces (endBy feature semi)
  fn <- getState
  return $ Class{className=newSymbol name,classParent=newSymbol pname,
                 classFeatures=fs,classFilename=newSymbol fn}
  <?> "class"

feature :: CoolParser Feature
feature = try (do
  name <- obj_id
  colon
  val_type <- type_id
  init <- option (mkE NoExpr) (do { reservedOp "<-"; expr })
  return $ Attr{attrName=newSymbol name,attrType=newSymbol val_type,
                attrInit=init})
  <|> do
  name <- obj_id
  args <- parens (sepBy formal comma)
  colon
  ret_type <- type_id
  body <- braces expr
  return $ Method{methodName=newSymbol name,methodFormals=args,
                  methodReturnType=newSymbol ret_type,methodExpr=body}
  <?> "feature"

formal :: CoolParser Formal
formal = do
  fname <- obj_id
  colon
  ftype <- type_id
  return $ Formal{formalName=newSymbol fname,formalType=newSymbol ftype}
  <?> "formal"

expr :: CoolParser Expression
expr = 
  try (buildExpressionParser operatorTable term)
  <|> do
  { name <- obj_id; reservedOp "<-"; body <- expr;
    return $ mkE Assign{assignName=newSymbol name,assignExpr=body} }
  -- <|> do
  -- { e <- option (mkE Object{objectName=newSymbol "self"}) (do e <- expr; dot; return e);
  --   name <- obj_id; args <- parens (sepBy expr comma);
  --   return $ mkE Dispatch{dispatchExpr=e,dispatchName=newSymbol name,dispatchActual=args}}
  -- <|> do
  -- { e <- expr; reservedOp "@"; typ <- type_id; dot; name <- obj_id;
  --   args <- parens (sepBy expr comma);
  --   return $ mkE StaticDispatch{staticDispatchExpr=e,staticDispatchType=newSymbol typ,
  --                               staticDispatchName=newSymbol name,staticDispatchActual=args}}
  <|> do
  { reserved "if"; e1 <- expr;
    reserved "then"; e2 <- expr;
    reserved "else"; e3 <- expr;
    reserved "fi";
    return $ mkE Cond{condPred=e1,condThen=e2,condExpression=e3}}
  <|> do
  { reserved "while"; e1 <- expr;
    reserved "loop"; e2 <- expr;
    reserved "pool";
    return $ mkE Loop{loopPred=e1,loopBody=e2}}
  <|> do
  { e <- braces (endBy expr semi);
    return $ mkE Block{blockBody=e}}
  <|> do
  { reserved "let";
    is <- many1 ( do id <- obj_id; colon;
                     typ <- type_id;
                     e <- option (mkE NoExpr) (do reservedOp "<-"; e <- expr; return e);
                     return (id, typ, e) );
    reserved "in";
    e <- expr;
    return $ mkLetBody is e }
  <|> do
  { reserved "case"; e <- expr; reserved "of";
    l <- many (do name <- obj_id; colon; typ <- type_id; reservedOp "=>"; e <- expr; semi;
                  return Branch{branchName=newSymbol name,branchType=newSymbol typ,branchExpr=e});
    reserved "esac";
    return $ mkE TypCase{typCaseExpr=e,typCaseCases=l}}
  <|> do
  { reserved "new"; typ <- type_id; return $ mkE New{newType=newSymbol typ}}
  <?> "expr"

operatorTable :: [[Operator String UserData ParseMonad Expression]]
operatorTable =
  [ [prefix "~" (\e -> mkE $ Neg{negE=e})],
    [prefix "isvoid" (\e -> mkE $ IsVoid{isVoidE=e})],
    [binary "*" (\e1 e2 -> mkE $ Mul{mulE1=e1,mulE2=e2}) AssocLeft,
     binary "/" (\e1 e2 -> mkE $ Divide{divideE1=e1,divideE2=e2}) AssocLeft],
    [binary "+" (\e1 e2 -> mkE $ Plus{plusE1=e1,plusE2=e2}) AssocLeft,
     binary "-" (\e1 e2 -> mkE $ Sub{subE1=e1,subE2=e2}) AssocLeft],
    [binary "<=" (\e1 e2 -> mkE $ Le{leE1=e1,leE2=e2}) AssocLeft,
     binary "<" (\e1 e2 -> mkE $ Lt{ltE1=e1,ltE2=e2}) AssocLeft,
     binary "=" (\e1 e2 -> mkE $ Eq{eqE1=e1,eqE2=e2}) AssocLeft],
    [prefix "not" (\e -> mkE $ Comp{compE=e})] ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })

term :: CoolParser Expression
term = parens expr
       <|> do { name <- obj_id; return $ mkE Object{objectName=newSymbol name}}
       <|> do { s <- intLit; sy <- addStringP s; return $ mkE IntConst{intConstToken=sy}}
       <|> do { s <- stringLit; sy <- addStringP s; return $ mkE StringConst{stringConstToken=sy}}
       <|> do { b <- boolLit; return $ mkE BoolConst{boolConstVal=b}}
       <?> "term"
       
-- TODO: error handling       

cCase :: CoolParser Case
cCase = do
  name <- obj_id
  colon
  typ <- type_id
  reservedOp "=>"    
  body <- expr
  return $ Branch{branchName=newSymbol name,branchType=newSymbol typ,branchExpr=body}

ctokens :: T.GenTokenParser String UserData ParseMonad
ctokens = T.makeTokenParser coolDef

coolDef = T.LanguageDef {
  commentStart = "(*",
  commentEnd   = "*)",
  commentLine = "--",
  nestedComments = True,
  identStart = letter <|> char '_',
  identLetter = alphaNum <|> char '_',
  opStart = oneOf "=+-*/%&^|<>!~@",
  opLetter = oneOf "->=",
  reservedNames = ["class","else","fi","if","in","inherits","isvoid","let","loop","pool",
                   "then","while","case","esac","new","of","not"],
  reservedOpNames = ["<-","+","-","*","/","<","<=","=","=>","~","@"],
  caseSensitive = False
                      }
          
whiteSpace :: CoolParser ()          
whiteSpace = T.whiteSpace ctokens
reserved :: String -> CoolParser ()
reserved = T.reserved ctokens
reservedOp :: String -> CoolParser ()
reservedOp = T.reservedOp ctokens
comma :: CoolParser ()
comma = do _ <- T.comma ctokens; return ()
semi :: CoolParser ()
semi = do _ <- T.semi ctokens; return ()
colon :: CoolParser ()          
colon = do _ <- T.colon ctokens; return ()
dot :: CoolParser ()           
dot = do _ <- T.dot ctokens; return ()
type_id :: CoolParser String
type_id =
  (lexeme ctokens $ try $
   do c <- upper
      cs <- many (identLetter coolDef)
      return $ c:cs)
  <?> "type_id"
obj_id :: CoolParser String
obj_id =
  (lexeme ctokens $ try $
   do c <- lower
      cs <- many (identLetter coolDef)
      return $ c:cs)
  <?> "obj_id"
parens :: CoolParser a -> CoolParser a
parens = T.parens ctokens
braces :: CoolParser a -> CoolParser a
braces = T.braces ctokens
true :: CoolParser Bool
true = do char 't'
          oneOf "rR"
          oneOf "uU"
          oneOf "eE"
          return True
false :: CoolParser Bool
false = do char 'f'
           oneOf "aA"
           oneOf "lL"
           oneOf "sS"
           oneOf "eE"
           return False
stringLit :: CoolParser String
stringLit = T.stringLiteral ctokens
intLit :: CoolParser String
intLit = many1 digit
boolLit :: CoolParser Bool
boolLit = try (true >> return True)
          <|> try (false >> return False)

mkE :: ExpressionBody -> Expression
mkE e = Expression e NoType

mkLetBody :: [(String, String, Expression)] -> Expression -> Expression     
mkLetBody [] ef = ef
mkLetBody ((id,typ,init):las) ef =
  mkE Let{letId=newSymbol id,letType=newSymbol typ,letInit=init,letBody=mkLetBody las ef}
