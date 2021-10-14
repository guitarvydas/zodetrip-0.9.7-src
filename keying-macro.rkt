#lang racket/gui
(provide keying)
(require "sv.rkt" "av.rkt" "move.rkt")

(define-syntax (switch stx)
  (define (transform-clause cl)
    (syntax-case cl (default)
      ((default expr) #'(else expr))
      ((val ... expr) #'((val ...) expr))))

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
    ((_ x clause ...)
     (with-syntax (((case-clause ...) (transform-clauses #'(clause ...))))
       #'(case x case-clause ...)))))

(define (keying w k)
  "( w k -- w) Process key events."

  (switch k
    [" " (sv w 'message #f)]
    ["\r" (sv w 'message #f)]
    
    ["m" (sv w 'map (not (av w 'map)))]

    ["8"  (writeln w) w]                           ; Write out current state.
    ["9"  (sv w 'dbg-trap (not (av w 'dbg-trap)))] ; Show traps.
    ["0" (sv w 'dbg-tile (not (av w 'dbg-tile)))] ; Tile identification mode.
    ["1" (sv w 'grid (not (av w 'grid)))]         ; Display tile grid overlay.
    ["2" (sv w 'debug (not (av w 'debug)))]       ; Display current state.

    ["right" (world-move w 1 0)]
    ["left"  (world-move w -1 0)]
    ["up"    (world-move w 0 -1)]
    ["down"  (world-move w 0 1)]

    ["wheel-up" (world-scroll w 1)]
    ["wheel-down" (world-scroll w -1)]

    [else w]
    )
  )

(define (simple-test)
  (expand-once '(switch k
    ["m" (println "m")]
    ["up" (println "up")]
    [default (println "DEFAULT")]
    )))

(define (big-test)
  (expand-once '(switch k
    [" " (sv w 'message #f)]
    ["\r" (sv w 'message #f)]
    
    ["m" (sv w 'map (not (av w 'map)))]

    ["8"  (writeln w) w]                           ; Write out current state.
    ["9"  (sv w 'dbg-trap (not (av w 'dbg-trap)))] ; Show traps.
    ["0" (sv w 'dbg-tile (not (av w 'dbg-tile)))] ; Tile identification mode.
    ["1" (sv w 'grid (not (av w 'grid)))]         ; Display tile grid overlay.
    ["2" (sv w 'debug (not (av w 'debug)))]       ; Display current state.

    ["right" (world-move w 1 0)]
    ["left"  (world-move w -1 0)]
    ["up"    (world-move w 0 -1)]
    ["down"  (world-move w 0 1)]

    ["wheel-up" (world-scroll w 1)]
    ["wheel-down" (world-scroll w -1)]

    [else w]
    )))

(define (big-test-run)
  (let ((k "2"))
    (switch k
            [" " (println "SPACE")]
            ["\r" (println "RETURN")]
            
            ["m" (println "m")]
            
            ["8"  (println "8")]
            ["9"  (println "9")]
            ["0" (println "0")]
            ["1" (println "1")]
            ["2" (println "2")]
            
            ["right" (println "right")]
            ["left"  (println "left")]
            ["up"    (println "up")]
            ["down"  (println "down")]
            
            ["wheel-up" (println "wheel up")]
            ["wheel-down" (println "wheel down")]
            
            [default (println "OOPS")]
            )))