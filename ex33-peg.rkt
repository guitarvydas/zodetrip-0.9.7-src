#lang peg
_ < [ \n]*;
psymbol <- res:(![() \n] .)+ _ -> (string->symbol res);
psexp <- s:psymbol / ~'(' s:psexp* ~')' _ -> s;