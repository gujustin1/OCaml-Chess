MODULES=chess_board command tile main
OBJECTS=$(MODULES:=.cmo)

MLS=$(MODULES:=.ml)
MLIS= chess_board.mli command.mli tile.mli
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS) 

test:
	$(OCAMLBUILD) -tag 'debug' test1.byte && ./test1.byte -runner sequential;
	$(OCAMLBUILD) -tag 'debug' test2.byte && ./test2.byte -runner sequential;
	$(OCAMLBUILD) -tag 'debug' test3.byte && ./test3.byte -runner sequential;
	$(OCAMLBUILD) -tag 'debug' test4.byte && ./test4.byte -runner sequential;
	$(OCAMLBUILD) -tag 'debug' test5.byte && ./test5.byte -runner sequential

js:
	dune build --profile release ./draw.bc.js

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

zip:
	zip chess.zip *.ml* *.txt *.sh _tags .merlin .ocamlformat .ocamlinit Makefile	dune dune-project index.html images/BlackBishop.png images/BlackKing.png images/BlackKnight.png images/BlackPawn.png images/BlackQueen.png images/BlackRook.png images/WhiteBishop.png images/WhiteKing.png images/WhiteKnight.png images/WhitePawn.png images/WhiteQueen.png images/WhiteRook.png
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,Js_of_ocaml \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,Js_of_ocaml \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private chess.zip