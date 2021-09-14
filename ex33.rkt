#lang racket
(require peg)
 
(define-peg/drop _ (* (or #\space #\newline)))
 
(define-peg symbol
  (and (name res (+ (and (! #\( #\) #\space #\newline) (any-char)))) _)
  (string->symbol res))
 
(define-peg/bake sexp
  (or symbol
      (and (drop #\() (* sexp) (drop #\) _))))

(define-peg ubsexp
  (or symbol
      (and (drop #\() (* sexp) (drop #\) _))))

(peg sexp "(foob (ar baz)quux)")
(peg ubsexp "(foob (ar baz)quux)")

(peg sexp "((())(()(())))")
(peg ubsexp "((())(()(())))")

(peg sexp "(lambda (x) (list x (list (quote quote) x)))")
(peg ubsexp "(lambda (x) (list x (list (quote quote) x)))")