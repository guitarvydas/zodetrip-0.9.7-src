#lang racket/gui
(provide keying)
(require "sv.rkt" "av.rkt" "move.rkt"
         macro-debugger/stepper
;         "peg-keying.rkt"
)

#|
(define (original-keying w k)
  "( w k -- w) Process key events."

  (case k
    [(" ") (sv w 'message #f)]
    [("\r") (println (sv w 'message #f)]
    
    [("m") (sv w 'map (not (av w 'map)))]

    [("8")  (writeln w) w]                           ; Write out current state.
    [("9")  (sv w 'dbg-trap (not (av w 'dbg-trap)))] ; Show traps.
    [("0") (sv w 'dbg-tile (not (av w 'dbg-tile)))] ; Tile identification mode.
    [("1") (sv w 'grid (not (av w 'grid)))]         ; Display tile grid overlay.
    [("2") (sv w 'debug (not (av w 'debug)))]       ; Display current state.

    [("right") (world-move w 1 0)]
    [("left")  (world-move w -1 0)]
    [("up")    (world-move w 0 -1)]
    [("down")  (world-move w 0 1)]

    [("wheel-up") (world-scroll w 1)]
    [("wheel-down") (world-scroll w -1)]

    [else w]
    )
  )
|#

(define (raw-keying w k)
  "( w k -- w) Process key events."

  (case k
    [("\r") (println "raw") (sv w 'message #f)]
    
    [("right") (println "raw right") (world-move w 1 0)]
    [("left")  (println "raw left") (world-move w -1 0)]
    [("up")    (println "raw up") (world-move w 0 -1)]
    [("down")  (println "raw down") (world-move w 0 1)]


    [else (println "raw else") w]
    )
  )

;;;;;;;;;

;; see https://stackoverflow.com/questions/32793972/racket-switch-statement-macro

(require (for-syntax syntax/parse))

;;;;;;;;; macro v1

(define-syntax (on-key-1 stx)
  (define (transform-clause cl)
    (syntax-case cl (default)
      ((default expr) #'(else expr))
      ((val sexpr ...) #'((val) sexpr ...))))

  (define (transform-clauses cls)
    (syntax-case cls ()
      ((cl)
       (with-syntax ((case-clause (transform-clause #'cl)))
         #'(case-clause)))
      ((cl rest ...)
       (with-syntax ((case-clause (transform-clause #'cl))
                     ((case-rest ...) (transform-clauses #'(rest ...))))
         #'(case-clause case-rest ...)))))

  (syntax-case stx ()
    ((_ k clause ...)
     (with-syntax (((case-clause ...) (transform-clauses #'(clause ...))))
       #'(case k case-clause ...)))))

(define (macro-1-keying w k)
  (on-key-2 k
    ["\r" (println "macro-1-enter") (sv w 'message #f)]
    
    ["right" (println "macro-1 right") (world-move w 1 0)]
    ["left"  (println "macro-1 left") (world-move w -1 0)]
    ["up"    (println "macro-1 up") (world-move w 0 -1)]
    ["down"  (println "macro-1 down") (world-move w 0 1)]


    [else (println "macro-2 else") w]
    )
  )

(define (test1)
  (expand-once '(on-key-1 k ["right" a b])))

;;;;;;;;; macro v2

(define-syntax (on-key-2 stx)
  (define (transform-clause cl)
    (syntax-case cl (default enter right left up down)
      ((default expr ...) #'(else expr ...))
      ((enter expr ...) #'(("\r") expr ...))
      ((right expr ...) #'(("right") expr ...))
      ((left expr ...) #'(("left") expr ...))
      ((up expr ...) #'(("up") expr ...))
      ((down expr ...) #'(("down") expr ...))
      ((val expr ...) #'((val) expr ...))))

  (define (transform-clauses cls)
    (syntax-case cls (enter)
      ((cl)
       (with-syntax ((case-clause (transform-clause #'cl)))
         #'(case-clause)))
      ((cl rest ...)
       (with-syntax ((case-clause (transform-clause #'cl))
                     ((case-rest ...) (transform-clauses #'(rest ...))))
         #'(case-clause case-rest ...)))))

  (syntax-case stx ()
    ((_ k clause ...)
     (with-syntax (((case-clause ...) (transform-clauses #'(clause ...))))
       #'(case k case-clause ...)))))

(define (macro-2-keying w k)
  (on-key-2 k
    [enter (println "macro-2-enter") (sv w 'message #f)]
    
    [right (println "macro-2-right") (world-move w 1 0)]
    [left  (println "macro-2 left") (world-move w -1 0)]
    [up    (println "macro-2 up") (world-move w 0 -1)]
    [down  (println "macro-2 down") (world-move w 0 1)]


    [else (println "macro-2 else") w]
    )
  )

(define (test2)
  (expand-once '(on-key-2 k
                          [enter a b]
                          [down c d]
                          )))

;;;;;;;;;

;;;;;;;;; macro v-3

(define-syntax (on-key-3 stx)
  (define (transform-clause cl)
    (syntax-case cl (default enter right left up down)
      ((default expr ...) #'(else expr ...))
      ((enter s expr ...) #'((s) expr ...))
      ((right s expr ...) #'((s) expr ...))
      ((left s expr ...) #'((s) expr ...))
      ((up s expr ...) #'((s) expr ...))
      ((down s expr ...) #'((s) expr ...))
      ((val expr ...) #'((val) expr ...))))

  (define (transform-clauses cls)
    (syntax-case cls (enter)
      ((cl)
       (with-syntax ((case-clause (transform-clause #'cl)))
         #'(case-clause)))
      ((cl rest ...)
       (with-syntax ((case-clause (transform-clause #'cl))
                     ((case-rest ...) (transform-clauses #'(rest ...))))
         #'(case-clause case-rest ...)))))

  (syntax-case stx ()
    ((_ k clause ...)
     (with-syntax (((case-clause ...) (transform-clauses #'(clause ...))))
       #'(case k case-clause ...)))))

(define (macro-3-keying w k)
  (on-key-3 k
    [enter "\r" (println "macro-3-enter") (sv w 'message #f)]
    
    [right "right" (println "macro-3-right") (world-move w 1 0)]
    [left  "left" (println "macro-3 left") (world-move w -1 0)]
    [up    "up" (println "macro-3 up") (world-move w 0 -1)]
    [down  "down" (println "macro-3 down") (world-move w 0 1)]


    [else (println "macro-3 else") w]
    )
  )

(define (test-3)
  (expand-once '(on-key-3 k
                          [enter "\r" a b]
                          [down "down" c d]
                          [default e f]
                          )))

;;;;;;;;;

;;;;;;;;; test 4 - using PEG + macro
(require "peg-keying.rkt")
;;;;;;;;;;

(define (keying w k)
  ;(macro-1-keying w k))
  ;(macro-2-keying w k))
  (macro-3-keying w k))
