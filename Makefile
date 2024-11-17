UTOP = /usr/bin/utop

PROGRAMS = fonction essai

all: $(PROGRAMS) 

essai : essai.ml 
	ocamlc -o essai essai.ml 

fonction : polynome.cmo fonction.ml
	ocamlc -o fonction polynome.cmo fonction.ml 

polynome.cmo : polynome.ml
	ocamlc -c  polynome.ml

utop: all
				 @$(UTOP) -init ocamlinit.ml

clean:
	rm -f *.cmo *.cmi $(PROGRAMS)