GHC=ghc
LEX=alex
PARSE=happy

LEX_SOURCE = Lexer.x
PARSE_SOURCE = Parser.y

default: lexer

lexer: src/Main.hs src/Lexer.x
	cd src; $(LEX) $(LEX_SOURCE)
	$(GHC) -o lexer src/$(LEX_SOURCE:.x=.hs) src/Main.hs

clean:
	/bin/rm -f *.o
	/bin/rm -f *.hi
	/bin/rm lexer