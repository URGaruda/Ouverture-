PROGRAMS = exp

all: $(PROGRAMS)

exp: code.ml
	ocamlc -o code code.ml

clean:
	rm -f *.cmo *.cmi $(PROGRAMS)
