#lang racket
(require peg)
(define-peg number (name res (+ (range #\0 #\9)))
  (string->number res))
(define-peg sum
  (and (name v1 prod) (? (and #\+ (name v2 sum))))
  (if v2 (+ v1 v2) v1))
(define-peg prod
  (and (name v1 number) (? (and #\* (name v2 prod))))
  (if v2 (* v1 v2) v1))

(define (test32)
  (peg sum "7*2+3*4"))