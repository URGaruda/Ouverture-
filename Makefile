UTOP = /usr/bin/utop

PROGRAMS = fonction essai arbre abr

all: $(PROGRAMS) 

abr : abr.ml
	ocamlc -o abr abr.ml 

arbre : polynome.cmo fonction.cmo arbre.ml
	ocamlc -o arbre polynome.cmo fonction.cmo arbre.ml

essai : essai.ml 
	ocamlc -o essai essai.ml 

fonction : polynome.cmo fonction.ml
	ocamlc -o fonction polynome.cmo fonction.ml 

polynome.cmo : polynome.ml
	ocamlc -c  polynome.ml

fonction.cmo : fonction.ml
	ocamlc -c fonction.ml 


utop: all
				 @$(UTOP) -init ocamlinit.ml

clean:
	rm -f *.cmo *.cmi $(PROGRAMS)