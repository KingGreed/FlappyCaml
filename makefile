NAME=FlappyCaml
TARGET_DIR=target/
VERSION=0.0.1-SNAPSHOT

ARTIFACT=$(TARGET_DIR)$(NAME)-$(VERSION)

DIR=src/

CC=clang
CFLAGS= -std=c99 -O2 -I`ocamlc -where` -lnjgr
OFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa

main: clean init $(DIR)main.ml $(DIR)network.o
	ocamlopt.opt $(OFLAGS) $(OCAMLLD) -o $(ARTIFACT) $(DIR)network.o $(DIR)main.ml

init:
	mkdir target

.SUFFIXES: .ml .cmx

.ml.cmx:
	ocamlopt.opt $(OFLAGS) $(OCAMLLD) -c $<

.o:
	$(CC) $(CFLAGS) $<

clean::
	rm -f *~ *.o *.cm[iox]
	rm -Rf $(TARGET_DIR)

