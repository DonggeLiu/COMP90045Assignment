# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     COMP90045 Programming Language Implementation, Programming Project      #
#                                                                             #
#                               GOAT - MAKEFILE                               #
#                                                                             #
# Well-chosen team name:              pli-dream-team-twentee-nineteen         #
# Well-chosen team members:                                                   #
# * Alan Ung                          alanu                                   #
# * David Stern                       dibstern                                #
# * Dongge Liu                        donggel                                 #
# * Mariam Shahid                     mariams                                 #
# * Matthew Farrugia-Roberts          farrugiam                               #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Goat: *.hs GoatLang/*.hs Util/*.hs
	ghc Goat.hs

submission: clean *.hs GoatLang/*.hs Util/*.hs _SubmissionMakefile Makefile
	mkdir submission
	tar -cvf submission/src.tar Goat.hs GoatLang/*.hs Util/*.hs
	cp _SubmissionMakefile submission/Makefile

tests: Goat utests itests

# Unit tests with HUnit:
tests/TestGen: tests/TestGen.hs
	ghc tests/TestGen.hs
utests: Goat tests/TestGen
	# ** RUN UNIT TESTS **
	tests/TestGen
clean_utests:
	rm -rf tests/*.hi tests/*.o tests/UnitTests tests/UnitTests.hs

# Integration tests:

# Recursive directory dependencies:
# (adapted from https://stackoverflow.com/a/25043823)
SAMPLE_ROOT = tests/samples
SAMPLE_DIRS = $(shell find $(SAMPLE_ROOT) -type d)
ALL_SAMPLES = $(shell find $(SAMPLE_ROOT) -type f -name '*.gt.*')
itests: Goat tests/testall.sh $(SAMPLE_ROOT) $(SAMPLE_DIRS) $(ALL_SAMPLES)
	# ** RUN INTEGRATION TESTS **
	bash tests/testall.sh
clean_itests:
	# rm -rf # (nothing to clean for integration tests)


clean: clean_utests clean_itests
	rm -rf *.o *.hi */*.o */*.hi Goat submission

.PHONY: clean tests itests clean_itests utests clean_utests submission
