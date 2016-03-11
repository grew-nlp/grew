PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin
DATA_DIR=$(PREFIX)/share/grew/

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

VERSION = `cat VERSION`

all: native

native: src/grew_glade.ml datadir
	$(OCB) -tag-line "true:	package(libgrew)" grew_main.native

datadir:
	echo $(DATA_DIR) > DATA_DIR

install:
	cp grew_main.native $(BINDIR)/grew
	mkdir -p $(DATA_DIR)
	cp src/grew.glade $(DATA_DIR)

uninstall:
	rm -f $(BINDIR)/grew
	rm -f $(DATA_DIR)/grew.glade

.PHONY:	all clean byte native install uninstall

clean:
	$(OCB) -clean
	rm -f DATA_DIR
	rm -f src/grew_glade.ml

info:
	@echo "BINDIR   = $(BINDIR)" 
	@echo "DATA_DIR = $(DATA_DIR)" 

# glade file are not handle by ocamlbuild
src/grew_glade.ml : src/grew.glade
	lablgladecc2 $< > $@
	sed -iback 's|src/grew.glade|$(DATA_DIR)grew.glade|g' src/grew_glade.ml
	rm -f src/grew_glade.mlback
