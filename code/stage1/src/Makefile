Goat: *.hs */*.hs
	ghc Goat.hs

tests: testgen.py
	python3 testgen.py > Tests.hs
	ghc Tests.hs
	./Tests

clean:
	rm -f *.o *.hi */*.o */*.hi Goat Tests Tests.hs

