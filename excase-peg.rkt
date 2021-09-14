#lang peg
(require "../racket-peg/s-exp.rkt");
(define (compile-snippet e) (eval e));
(define (ptest k)
  (let ((pexpr (peg pcasetest "1 'a $ 2 'b $ 3 'c $ 4 'd $ else 'e")))
    (let ((e (compile-snippet pexpr)))
      (println (e k)))));

pclause <- caselabel:s-exp _ e:s-exp -> (list (list caselabel) e);
pelseclause <- 'else' _ (e:s-exp) -> (list 'else e);
poneclause <- pelseclause / pclause;
pclauses <- cl1:poneclause (_ '$' _ cl2:pclauses)? -> (if cl2 (cons cl1 cl2) (list cl1));
pcasetest <- cls:pclauses -> (list 'lambda '(k) (cons 'case (cons 'k cls)));
