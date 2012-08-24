GHC=ghc
GHCOPTS=-XTypeSynonymInstances -XUndecidableInstances -XMultiParamTypeClasses -XFlexibleInstances -i src/Syntax.hs -i src/SCUtil.hs -i src/Type.hs -XRankNTypes -XFlexibleContexts
LEX=alex
LEXOPTS=--ghc --info
PARSE=happy
PARSEOPTS=--ghc --info

default: semant src/Syntax.hs src/SCUtil.hs src/Type.hs

lexer: src/LexMain.hs src/Lexer.x
	cd src; $(LEX) $(LEXOPTS) Lexer.x
	$(GHC) $(GHCOPTS) -o lexer src/Lexer.hs src/LexMain.hs

parser: src/ParseMain.hs src/Parser.y lexer src/ParserPrettyPrint.hs
	cd src; $(PARSE) $(PARSEOPTS) Parser.y
	$(GHC) $(GHCOPTS) -o parser src/ParseMain.hs src/Parser.hs src/Lexer.hs src/ParserPrettyPrint.hs

parsec: src/ParserParsec.hs src/ParserPrettyPrint.hs src/ParsecMain.hs
	$(GHC) $(GHCOPTS) -o parser src/ParsecMain.hs src/ParserParsec.hs src/ParserPrettyPrint.hs

semant: src/SemantMain.hs src/Semant.hs lexer parser
	$(GHC) $(GHCOPTS) -o semant src/Semant.hs src/SemantMain.hs src/Parser.hs src/ParserPrettyPrint.hs src/Lexer.hs

debug: GHCOPTS += -debug
debug: LEXOPTS += -d
debug: PARSEOPTS += -ad
debug: default;

clean:
	/bin/rm -f src/*.o
	/bin/rm -f src/*.hi
	/bin/rm -f src/Lexer.hs
	/bin/rm -f src/Parser.hs
	/bin/rm -f lexer
	/bin/rm -f parser
	/bin/rm -f semant
