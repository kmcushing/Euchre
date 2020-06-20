MODULES = state command settings main deck authors
OBJECTS = $(MODULES:=.cmo)
MLS = $(MODULES:=.ml)
MLIS = $(MODULES:=.mli)
TEST = test.byte
MAIN = main.byte
OCAMLBUILD = ocamlbuild -use-ocamlfind
PKGS = ANSITerminal


default: build
				utop
	
build:
				$(OCAMLBUILD) $(OBJECTS)

test:
				$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

docs:	docs-public docs-private

docs-public: build
				mkdir -p doc.public
				ocamlfind ocamldoc -I _build -package $(PKGS) \
					-html -stars -d doc.public $(MLIS)

docs-private: build
				mkdir -p doc.private
				ocamlfind ocamldoc -I _build -package $(PKGS) \
					-html -stars -d doc.private \
					-inv-merge-ml-mli -m A $(MLIS) $(MLS)

zip:
				zip ms2.zip *.ml* _tags Makefile INSTALL.txt

play:
				$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
				ocamlbuild -clean
				rm -rf doc.public doc.private ms2.zip
