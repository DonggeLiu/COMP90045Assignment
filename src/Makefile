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

SOURCES=*.hs Util/*.hs GoatLang/*/*.hs OzLang/*.hs
Goat: $(SOURCES)
	ghc Goat.hs

submission: clean $(SOURCES) _SubmissionMakefile Makefile
	mkdir submission
	tar -cvf submission/src.tar Goat.hs GoatLang/*.hs Util/*.hs
	cp _SubmissionMakefile submission/Makefile

# Automated tests:

tests: Goat utests itests

# Unit tests with HUnit:
tests/UnitTests: tests/UnitTests.hs
	ghc tests/UnitTests.hs
utests: Goat tests/UnitTests
	# ** RUN UNIT TESTS **
	tests/UnitTests
clean_utests:
	rm -rf tests/*.hi tests/*.o tests/UnitTests


# Integration tests:
# Recursive directory dependencies (adapted from
# https://stackoverflow.com/a/25043823)
SAMPLE_ROOT = tests/samples
SAMPLE_DIRS = $(shell find $(SAMPLE_ROOT) -type d)
ALL_SAMPLES = $(shell find $(SAMPLE_ROOT) -type f -name '*.gt.*')
itests: Goat tests/testall.sh $(SAMPLE_ROOT) $(SAMPLE_DIRS) $(ALL_SAMPLES)
	# ** RUN INTEGRATION TESTS **
	bash tests/testall.sh


# Stage 3 milestones:

MILES_DIR = tests/samples/stage3-miles
miles: Goat tests/milestones.sh $(MILES_DIR) 
	bash tests/milestones.sh


# Makefile stuff:

clean: clean_utests
	rm -rf *.o *.hi */*.o */*.hi */*/*.o */*/*.hi Goat submission

.PHONY: clean tests itests utests clean_utests submission miles
