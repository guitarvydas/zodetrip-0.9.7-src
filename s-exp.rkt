#lang peg
_ <- [ \n]*;
symbol <- res:(![() \n] .)+ _ -> (string->symbol res);
s-exp <- s:symbol / ~'(' s:s-exp* ~')' _ -> s;
