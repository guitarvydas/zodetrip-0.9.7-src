#lang peg
(require "../racket-peg/s-exp.rkt");
(define (compile-snippet e) (eval e));
(define (ptest k)
  (let ((pexpr (peg pcasetest "$ 1 'a $ 2 'b $ 3 'c $ 4 'd $ else 'f")))
    (println pexpr)
    (let ((e (compile-snippet pexpr)))
      (println (e k)))));

pclause <- '$' _ (! 'else') caselabel:s-exp _ e:s-exp _ -> (list (list caselabel) e);
pelseclause <- '$' _ 'else' _ e:s-exp -> (list 'else e);
pclauses <- cl1:pclause+ ecl:pelseclause? -> (append cl1 (list ecl));
pcasetest <- cls:pclauses -> (list 'lambda '(k) (cons 'case (cons 'k cls)));
