#lang racket/gui
(provide hero-face try-move hero-move world-move world-scroll)
(require "sv.rkt" "av.rkt" "qroom.rkt" "sound.rkt" "vector.rkt" "data.rkt")

(define (hero-face w dx)
  "( w dx dy -- w) What direction should hero face?"

  (sv w 'hero-face (cond [(< dx 0) 'left]
                         [(> dx 0) 'right]
                         [else (or (av w 'hero-face) 'right)])))

(define (try-move w p)
  "( w p -- w) Try to move hero to position p."

  ;; (printf "-- try-move w ~a~n" p) ; Debug.
  (let* [(px (first p))  (py (second p))
         (qr (av w 'qroom))                
         (size (qroom-size qr))]
    (if (or (> 0 px) (< (- (first size) 1) px)
            (> 0 py) (< (- (second size) 1) py)
            (not (list-ref (qroom-floor qr) (+ (* (first size) py) px))))
        w
        (begin
          (play-safe 'step) ; Side effect!!!
          (sv w 'hero-pos p)))))

(define (hero-move w dx dy)
  "( w dx dy -- w) Move (or don't) hero. Checks floor (later mobs too)."

  (if (and (eq? 0 dx) (eq? 0 dy)) w ; If we haven't moved, go on.
      (let* [(d (v+v (av w 'hero-pos) (list dx dy)))]
        (sv (try-move (hero-face w dx) d)
            'event (list 'hit d)))))

  
(define (world-move w dx dy)
  "( w dx dy -- w) Move appropriate world object by delta."

  (if (av w 'message) w
    (cond [(av w 'map)
           (sv w 'map-hex-c
               (modulo (+ (or (av w 'map-hex-c) 0) dy) (length MAP-ZODES)))]
          [(av w 'dbg-tile)
           (sv w 'dbg-tile-p
               (vroll (v+v (or (av w 'dbg-tile-p) (av w 'hero-pos)) (list dx dy))
                      (qroom-size (av w 'qroom))))]
          [else (hero-move w dx dy)])))
  
(define (world-scroll w dx)
  "( w dx -- w) Apply scroll wheel movements."

  (if (av w 'map)
    (sv w 'map-hex-r (+ dx (or (av w 'map-hex-r) 41)))
    w))


