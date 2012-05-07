GHC=ghc
LEX=alex
PARSE=happy

default: lexer

lexer: src/Main.hs src/Lexer.x
	cd src; $(LEX) Lexer.x
	$(GHC) -o lexer src/Lexer.hs src/Main.hs

clean:
	/bin/rm -f src/*.o
	/bin/rm -f src/*.hi
	/bin/rm -f src/Lexer.hs
	/bin/rm -f lexer
