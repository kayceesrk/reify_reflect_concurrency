all: rr_conc.native chameneos_monad.native chameneos_rr.native chameneos.native chameneos_shallow.native

rr_conc.native: rr_conc.ml
	ocamlbuild -pkg lwt.unix $@

%.native: %.ml
	ocamlbuild -cflag -g $@

clean:
	ocamlbuild -clean
	rm -f *~
