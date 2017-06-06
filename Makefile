all: chameneos_monad.native chameneos_rr.native chameneos.native chameneos_shallow.native

%.native: %.ml
	ocamlbuild -cflag -g $@

clean:
	ocamlbuild -clean
	rm -f *~
