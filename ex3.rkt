#lang peg
_ < [ \n]*;
symbol <- res:(![() \n] .)+ _ -> (string->symbol res);
sexp <- s:symbol / ~'(' s:sexp* ~')' _ -> s;
