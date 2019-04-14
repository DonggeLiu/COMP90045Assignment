Goat: *.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi */*.o */*.hi Goat

lexer: GoatLang/Lexer.x
	alex GoatLang/Lexer.x
	ghc GoatLang/Lexer.hs

parser: lexer GoatLang/LALRParser.y
	happy GoatLang/LALRParser.y
	ghc GoatLang/LALRParser.hs
