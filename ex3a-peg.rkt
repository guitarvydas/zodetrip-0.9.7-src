#lang peg
(define sentence "the quick brown fox jumps over the lazy dog");
(define (test1) (peg word sentence));
(define (test2) (peg manywords sentence));

non-space <- (! ' ') . ;
word <- c:(non-space+ ~(' ' ?)) -> c;
manywords <- c:word+ -> c;
