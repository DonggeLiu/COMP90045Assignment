TEAM=PLI-DREAM-TEAM-TWENTEE-NINETEEN
PROJ=stage1

Goat: *.hs */*.hs
	ghc Goat.hs

submit: *.hs */*.hs ExtractingMakefile Makefile
	mkdir -p submit
	tar -cvf submit/$(PROJ).tar.gz *.hs */*.hs # compress files for submission
	cp ExtractingMakefile submit/Makefile
	echo "submit everything inside the new directory 'submit/'."

clean:
	rm -f *.o *.hi */*.o */*.hi Goat
	rm -rf submit
