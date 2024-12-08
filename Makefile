PROGRAMS = exp

all: $(PROGRAMS)

exp: yann_experimentations.ml
	ocamlc -o projet yann_experimentations.ml 

clean:
	rm -f *.cmo *.cmi $(PROGRAMS)