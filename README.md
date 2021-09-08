[OCamlChess]
=============
OCamlChess is a HTML 5 Canvas implementation of Chess written entirely in OCaml.

## Building
1. Ensure these packages are installed js\_of\_ocaml (>= 3.9) js\_of\_ocaml\_lwt (>= 3.9) js\_of\_ocaml\_ppx (>= 3.9): 
   `opam install js_of_ocaml js_of_ocaml-lwt js_of_ocaml-ppx`
2. dune (>= 2.8.5) is required to build
   `opam install dune`
3. With Dune installed, run:
   `make build; make js`
4. Open `index.html` to run!
5. Alternatively, to run terminal version of chess, run:
   `make play`

## Instructions for terminal chess
`d2 d3` - attempt to move piece at (d, 2) to (d, 3)
`quit` - end the game

## Authors
* Chris Gu
* Justin Gu
* Tucker Stanley
