PROGRAMS = exp

all: $(PROGRAMS)

exp: code_final.ml
	ocamlc -o projet code_final.ml

clean:
	rm -f *.cmo *.cmi $(PROGRAMS)
