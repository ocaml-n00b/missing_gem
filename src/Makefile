COMP = ocamlopt
FLAGS = -strict-formats -safe-string

gems:
	$(COMP) $(FLAGS) -o missingGem common_def.ml sheet.ml list_fun.ml player.ml search_deck.ml game.ml mgem.ml

clean:
	rm *.cm*
	rm *.o
