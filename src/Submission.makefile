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

Goat: Goat.hs
	ghc Goat.hs

Harald: Harald.hs
	ghc Harald.hs

%.hs: src.tar
	tar -xvf src.tar
	touch $@

clean:
	rm -f *.o *.hi */*.o */*.hi */*/*.o */*/*.hi Goat Harald

.PHONY: src clean demo
