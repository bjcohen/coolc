module Parser.PrettyPrint where
import Syntax
import Parser
import qualified Data.Map as Map

class StanfordPP b where
  pp :: Map.Map Symbol String -> b -> [String]

prepend2 :: String -> String
prepend2 = ("  " ++)

instance StanfordPP a => StanfordPP [a] where    
  pp st cls = cls >>= (pp st)
    
instance StanfordPP Program where
  pp st Program{programClasses=cls} =
    ["#1", "_program"] ++ (map prepend2 $ pp st cls)

instance StanfordPP Class where    
  pp st Class{className=name,classParent=par,classFeatures=feat,classFilename=file} =
    ["#1", "_class"] ++
    (map prepend2 $ [show name, show par, show $ show file, "("] ++ pp st feat ++ [")"])
    
instance StanfordPP Feature where
  pp st Method{methodName=name,methodFormals=formals,methodReturnType=rt,methodExpr=expr} =
    ["#1", "_method"] ++ (map prepend2 $ [show name] ++ pp st formals ++ [show rt] ++ pp st expr)
  pp st Attr{attrName=name,attrType=at,attrInit=init} =
    ["#1", "_attr"] ++ (map prepend2 $ [show name, show at] ++ pp st init)
  
instance StanfordPP Formal where
  pp st Formal{formalName=name,formalType=typ} =
    ["#1", "_formal"] ++ (map prepend2 $ [show name, show typ])
  
instance StanfordPP Expression where  
  pp st (Expression b t) = (pp st b) ++ (pp st t)
  
instance StanfordPP ExpressionBody where
  pp st Assign{assignName=name,assignExpr=expr} =
    ["#1", "_assign"] ++ (map prepend2 $ [show name] ++ pp st expr) 
  pp st StaticDispatch{staticDispatchExpr=expr,staticDispatchType=typ,staticDispatchName=name,staticDispatchActual=actual} =
    ["#1", "_static_dispatch"] ++ (map prepend2 $ pp st expr ++ [show typ, show name] ++ ["("] ++ pp st actual ++ [")"]) 
  pp st Dispatch{dispatchExpr=expr,dispatchName=name,dispatchActual=actual} =
    ["#1", "_dispatch"] ++ (map prepend2 $ pp st expr ++ [show name] ++ ["("] ++ pp st actual ++ [")"]) 
  pp st Cond{condPred=e1,condThen=e2,condExpression=e3} =
    ["#1", "_cond"] ++ (map prepend2 $ pp st e1 ++ pp st e2 ++ pp st e3) 
  pp st Loop{loopPred=pred,loopBody=body} =
    ["#1", "_loop"] ++ (map prepend2 $ pp st pred ++ pp st body) 
  pp st TypCase{typCaseExpr=expr,typCaseCases=cases} =
    ["#1", "_typcase"] ++ (map prepend2 $ pp st expr ++ pp st cases) 
  pp st Block{blockBody=body} =
    ["#1", "_block"] ++ (map prepend2 $ pp st body) 
  pp st Let{letId=id,letType=typ,letInit=init,letBody=body} =
    ["#1", "_let"] ++ (map prepend2 $ [show id, show typ] ++ pp st init ++ pp st body) 
  pp st Plus{plusE1=e1,plusE2=e2} = ppBinary "_plus" e1 e2 st
  pp st Sub{subE1=e1,subE2=e2} = ppBinary "_sub" e1 e2 st
  pp st Mul{mulE1=e1,mulE2=e2} = ppBinary "_mul" e1 e2 st
  pp st Divide{divideE1=e1,divideE2=e2} = ppBinary "_divide" e1 e2 st
  pp st Neg{negE=e} = ["#1", "_neg"] ++ (map prepend2 $ pp st e) 
  pp st Lt{ltE1=e1,ltE2=e2} = ppBinary "_lt" e1 e2 st
  pp st Eq{eqE1=e1,eqE2=e2} = ppBinary "_eq" e1 e2 st
  pp st Le{leE1=e1,leE2=e2} = ppBinary "_leq" e1 e2 st
  pp st Comp{compE=e} = ["#1", "_comp"] ++ (map prepend2 $ pp st e) 
  pp st IntConst{intConstToken=t} =
    ["#1", "_int", "  " ++ st Map.! t]
  pp st BoolConst{boolConstVal=b} =
    ["#1", "_bool", "  " ++ (if b then "1" else "0")]
  pp st StringConst{stringConstToken=t} =
    ["#1", "_string", "  " ++ show (st Map.! t)]
  pp st New{newType=typ} =
    ["#1", "_new", "  " ++ show typ]
  pp st IsVoid{isVoidE=e} = ["#1", "_isvoid"] ++ (map prepend2 $ pp st e) 
  pp st NoExpr = ["#1", "_no_expr"]
  pp st Object{objectName=s} = ["#1", "_object", "  " ++ show s]

ppBinary :: String -> Expression -> Expression -> Map.Map Symbol String -> [String]
ppBinary name e1 e2 st =
  ["#1", name] ++ (map prepend2 $ pp st e1 ++ pp st e2) 
  
instance StanfordPP Case where
  pp st Branch{branchName=name,branchType=typ,branchExpr=expr} =
    ["#1", "_branch"] ++ (map prepend2 $ [show name, show typ] ++ pp st expr)

instance StanfordPP Type where
  pp st NoType = [": _no_type"]
  pp st (Type typ) = [": " ++ show typ]