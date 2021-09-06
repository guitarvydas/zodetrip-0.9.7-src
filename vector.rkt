#lang racket/gui
;; Vector Math
(provide v+v v*s vroll vlimit)

(define (v+v a b)
  "( a b -- c) Add two vectors together."

  (list (+ (first a) (first b)) (+ (second a) (second b))))

(define (v*s v s)
  "( v s -- v') Multiply vector v times scalar s."

  (list (* (first v) s) (* (second v) s)))

(define (vroll a vmax)
  "( a vmax -- a') Roll a around if outside of ((0 0) vmax)."

  (list (modulo (first a) (first vmax)) (modulo (second a) (second vmax))))

(define (vlimit a vmax)
  "( a vmax -- a') Limit a to range ((0 0) vmax)."

  (list (cond [(> 0 (first a)) 0] [(<= (first vmax) (first a)) (first vmax)] [else (first a)])
        (cond [(> 0 (second a)) 0] [(<= (second vmax) (second a)) (second  vmax)] [else (second a)])))
