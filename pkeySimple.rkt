#lang peg
(require "../racket-peg/s-exp.rkt");

pkeying <- _ v1:keyActionPair+ -> (append (list 'case 'k) v1);

key <- F9KEY / F10KEY;
F9KEY <-'f9' -> "f9";
F10KEY <-'f10' -> "f10";
keyActionPair <- k:key _ ':' _ a:s-exp _ -> (list (list k) a);
action <- a:s-exp -> a;
_ <- [ \n]*;
