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

;(define (compile-snippet e) (eval e))
(define (compile-snippet e)
  (let ((fake (list 'lambda '(w k) '(println "in fake"))))
    (println (format "in compile-snippet fake syntax? ~a, e pair? ~a"
                     (syntax? fake) (pair? fake)))
    (let ((fakep (eval fake)))
      (println (format "in compile-snippet fakep syntax? ~a, fakep pair? ~a, fakep proc? ~a"
                       (syntax? fakep) (pair? fakep) (procedure? fakep)))
      (println (format "in compile-snippet e syntax? ~a, e pair? ~a"
                       (syntax? e) (pair? e)))
      (let ((p (eval e)))
        (println (format "in compile-snippet p syntax? ~a, p pair? ~a, p proc? ~a"
                         (syntax? p) (pair? p) (procedure? p)))
        p))));

(define (test k)
  (let ((pexpr (peg casetest "1 'a $ 2 'b $ 3 'c $ 4 'd $ else 'e")))
    ;(println pexpr)
    (let ((e (compile-snippet pexpr)))
      (println (e k)))))
