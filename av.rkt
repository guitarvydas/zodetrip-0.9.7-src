#lang racket/gui
(provide av ar)
;; Association Lists
(define (av data key)
  "( data key -- value) Get value from association list."

  (when (not (list? data))
    (writeln (format "Bad list, requested: ~a" key)))
  (let ([found (assoc key data)])
    (if found (last found) #f)))

(define (ar data key)
  "( data key -- rest) Get rest of values from association list."

  (let ([found (assoc key data)])
    (if found (rest found) #f)))
