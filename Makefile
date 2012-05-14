GHC=ghc
GHCOPTS=-XTypeSynonymInstances -XDeriveDataTypeable
LEX=alex
LEXOPTS=--ghc --info
PARSE=happy
PARSEOPTS=--ghc --info

default: parser

lexer: src/LexMain.hs src/Lexer.x
	cd src; $(LEX) $(LEXOPTS) Lexer.x
	$(GHC) $(GHCOPTS) -o lexer src/Lexer.hs src/LexMain.hs

parser: src/ParseMain.hs src/Parser.y lexer src/Syntax.hs
	cd src; $(PARSE) $(PARSEOPTS) Parser.y
	$(GHC) $(GHCOPTS) -o parser src/ParseMain.hs src/Parser.hs src/Syntax.hs src/Lexer.hs

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
