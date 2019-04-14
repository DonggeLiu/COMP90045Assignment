all: Goat

Goat: Goat.hs */*.hs
	ghc Goat.hs

Bug: Bug.hs */*.hs
	ghc Bug.hs
GoatLang/Lexer.hs: GoatLang/Lexer.x
	alex GoatLang/Lexer.x
GoatLang/LALRParser.hs: GoatLang/Lexer.hs GoatLang/LALRParser.y
	happy GoatLang/LALRParser.y

clean:
	rm -f *.o *.hi */*.o */*.hi Goat Bug

