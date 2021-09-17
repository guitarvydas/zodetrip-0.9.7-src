#lang racket
(define msg '(println "hello"))

(define xlis (list 'lambda '() msg))

(define lis (list 'lambda '(w k) '(println "in wxy") 123))

(define stxlis (datum->syntax #f lis))

(define (test)
  (println lis)
  (println stxlis)
  (println (format "stxlis is syntax? ~a" (syntax? stxlis)))
  (let ((e (eval stxlis)))
    (e 1 2)))

