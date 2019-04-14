Goat: *.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi Goat

lexer: GoatLexer.x
	alex GoatLexer.x
	ghc GoatLexer.hs

parser: lexer GoatParser.y
	happy GoatParser.y
	ghc GoatParser.hs
