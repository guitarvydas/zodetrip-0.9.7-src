#lang racket

(require peg)
(require "../racket-peg/s-exp.rkt")

(define-peg clause
  (and (name caselabel s-exp) _ (name e s-exp))
  (list (list caselabel) e))

(define-peg elseclause
  (and "else" _ (name e s-exp))
  (list 'else e))

(define-peg clauses
  (and (name cl1 (or elseclause clause))
      (? _ #\$ _ (name cl2 clauses)))
    (if cl2
      (cons cl1 cl2)
      (list cl1)))

(define-peg casetest
  (name cls clauses)
  (list 'lambda '(k) (cons 'case (cons 'k cls))))

(define (compile-snippet e) (eval e))

(define (test k)
  (let ((pexpr (peg casetest "1 'a $ 2 'b $ 3 'c $ 4 'd $ else 'e")))
    ;(println pexpr)
    (let ((e (compile-snippet pexpr)))
      (println (e k)))))
