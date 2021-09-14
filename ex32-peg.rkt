#lang peg
(define (test32p) (peg psum "1*2+3*4"));

pnumber <- n:[0-9]+ -> (string->number n);
psum <- v1:pprod ('+' v2:psum)? -> (if v2 (+ v1 v2) v1);
pprod <- v1:pnumber ('*' v2:pprod)? -> (if v2 (* v1 v2) v1);
