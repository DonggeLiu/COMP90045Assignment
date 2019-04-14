all: Goat

Goat: Goat.hs */*.hs
	ghc Goat.hs

Bug: Bug.hs Goat lexer parser
	ghc Bug.hs
lexer: GoatLang/Lexer.x
	alex GoatLang/Lexer.x
parser: lexer GoatLang/LALRParser.y
	happy GoatLang/LALRParser.y

clean:
	rm -f *.o *.hi */*.o */*.hi Goat Bug

