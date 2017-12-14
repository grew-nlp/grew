PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

VERSION = `cat VERSION`

all: native

native:
	$(OCB) -tag-line "true:	package(libgrew)" grew_main.native

install:
	cp grew_main.native $(BINDIR)/grew

uninstall:
	rm -f $(BINDIR)/grew

.PHONY:	all clean byte native install uninstall

clean:
	$(OCB) -clean

info:
	@echo "BINDIR   = $(BINDIR)"
