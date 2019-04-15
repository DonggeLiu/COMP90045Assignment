Goat: *.hs GoatLang/*.hs Util/*.hs
	ghc Goat.hs

submission: clean *.hs GoatLang/*.hs Util/*.hs _SubmissionMakefile Makefile
	mkdir submission
	tar -cvf submission/src.tar.gz Goat.hs GoatLang/*.hs Util/*.hs
	cp _SubmissionMakefile submission/Makefile

tests: testgen.py
	python3 testgen.py > Tests.hs
	ghc Tests.hs
	./Tests

clean:
	rm -rf *.o *.hi */*.o */*.hi Goat submission Tests Tests.hs
