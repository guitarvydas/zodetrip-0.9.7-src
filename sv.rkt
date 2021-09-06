#lang racket/gui
(provide sv)
;; Could this be faster?
(define (sv data key value)
  "( data key value -- data') Set or create key with value."

  (if (assoc key data)
      (map (λ (x)
             (if (equal? key (first x))
                 (list key value)
                 x))
           data)
      (cons (list key value) data)))