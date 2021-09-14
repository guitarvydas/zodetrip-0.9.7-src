#lang racket/gui
(require peg)
(define sentence "the quick brown fox jumps over the lazy dog")
(define-peg non-space
  (and (! #\space) (any-char)))
(define-peg/bake word
  (and (+ non-space)
       (drop (? #\space))))
(peg word sentence)
(peg (+ word) sentence)

(define-peg countword
  (and (name w (+ non-space))
       (drop (? #\space)))
  (string-length w))

(peg (+ countword) sentence)