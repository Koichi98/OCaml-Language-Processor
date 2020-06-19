SOURCES = syntax.ml lexer.mll parser.mly tySyntax.ml constraintSolver.ml polymorphism.ml infer.ml eval.ml main.ml 
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library 

-include OCamlMakefile
