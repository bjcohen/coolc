GHC=ghc
LEX=alex
LEXOPTS=
PARSE=happy
PARSEOPTS=--ghc -i
default: lexer

lexer: src/LexMain.hs src/Lexer.x
	cd src; $(LEX) $(LEXOPTS) Lexer.x
	$(GHC) -o lexer src/Lexer.hs src/LexMain.hs

parser: src/ParseMain.hs src/Parser.y src/Lexer.hs
	cd src; $(PARSE) $(PARSEOPTS) Parser.y
	$(GHC) -o parser src/Parser.hs src/ParseMain.hs src/Lexer.hs

clean:
	/bin/rm -f src/*.o
	/bin/rm -f src/*.hi
	/bin/rm -f src/Lexer.hs
	/bin/rm -f src/Parser.hs
	/bin/rm -f lexer
	/bin/rm -f parser
