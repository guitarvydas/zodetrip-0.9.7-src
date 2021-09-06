#lang racket/gui

(require racket/runtime-path
         2htdp/universe 2htdp/image
         "data.rkt" "rooms.rkt" "stubs.rkt" "vector.rkt"
         "qroom.rkt" "sv.rkt" "av.rkt" "move.rkt" "sound.rkt"
         "keying.rkt")

(define APP '((name "Zodetrip")
              (nickname "zodetrip")
              (description "Restore the Endless Century!")
              (author "oofoe@cjmunday.com")
              (version "0.9.7")
              (license "GPL-3.0-or-later")))


#|
Copyright 2020 Jos'h Fuller

This file is part of Zodetrip.

Zodetrip is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Zodetrip is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Zodetrip.  If not, see <https://www.gnu.org/licenses/>.
|#


(define TILE-X 64)
(define TILE-Y 64)
(define WIDTH (* TILE-X 16))
(define HEIGHT (* TILE-Y 9))
(define VIEW (make-object bitmap% WIDTH HEIGHT))



(define-runtime-path-list PPICS
  '("i/sprites-02-4b.png"
    "i/map-01.png"))


(define PICS (for/list
                 ([s '(sprites zodemap)]
                  [p PPICS])
               (list s (read-bitmap p))))


;                                                                                                     
;                                                                                                     
;                                                                                                     
;                             ;                     ;                     ;                           
;                ;            ;       ;;;;          ;        ;            ;                           
;   ;;;   ;;;    ;                       ;                   ;                                        
;    ;     ;     ;                       ;                   ;                                        
;    ;     ;   ;;;;;;;;    ;;;;          ;       ;;;;      ;;;;;;;;    ;;;;        ;;;;       ;;;; ;  
;    ;     ;     ;            ;          ;          ;        ;            ;       ;    ;     ;    ;;  
;    ;     ;     ;            ;          ;          ;        ;            ;      ;      ;    ;        
;    ;     ;     ;            ;          ;          ;        ;            ;      ;      ;     ;;;;    
;    ;     ;     ;            ;          ;          ;        ;            ;      ;;;;;;;;         ;   
;    ;     ;     ;            ;          ;          ;        ;            ;      ;                 ;  
;    ;     ;     ;            ;          ;          ;        ;            ;      ;           ;     ;  
;     ;   ;      ;    ;;      ;          ;          ;        ;    ;;      ;       ;     ;    ;;   ;   
;      ;;;        ;;;;    ;;;;;;;;;  ;;;;;;;;;  ;;;;;;;;;     ;;;;    ;;;;;;;;;    ;;;;;     ; ;;;    
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     

;; Debugging
(define (mon x)
  "( x -- x) Debugging: Pass through dump variable while using it."

  (printf "mon: ~a~n" x) x)

(define (sh) "( --) Halt music."  (stub-halt-music)) ; Stop the music after crash...








;; Prolog -- Sorta'
(define (pq fb p s)
  "( fb /p s/ -- items) Query factbase fb."

  ;; (writeln (format "Requesting ~a & ~a..." p s)) ; Debug.
  (cond [(and p s) (filter (λ (x) (and (eq? p (first x)) (eq? s (second x)))) fb)]
        [p         (filter (λ (x) (eq? p (first x))) fb)]
        [s         (filter (λ (x) (eq? s (second x))) fb)]))

(define (pqv fb p s)
  "( fb /p s/ -- value|#f) Query factbase to get value. Sugar for pq."

  (let [(found (pq fb p s))]
    (if (and (not (empty? found)) (list? found))
             (last (last found)) #f)))

;; Graphics
(define (using dc pen brush)
  "( dc pen brush --) Set both pen and brush at same time."
  
  (send dc set-pen   (av PAINTBOX pen))
  (send dc set-brush (av PAINTBOX brush)))

(define (draw-circle dc x y r)
  "( dc x y r --) Draw circle."

  (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r)))

(define (draw-hex dc x y r a)
  "( dc x y r a --) Draw hexagon."

  (send dc draw-polygon
        (for/list [(i (range a (+ a (* 2 pi)) (/ pi 3)))]
          (cons (floor (* r (sin i))) (floor (* r (cos i)))))
        x y))

(define (sp name x y flip)
  "( name x y flip -- (name bitmap)) Build sprite."
  
  (let* [(b (make-object bitmap% 64 64))
         (dc (new bitmap-dc% [bitmap b]))
         (sx (if flip -2 2))
         (ox (if flip (- 32) 0))]
    (send dc scale sx 2)
    (send dc draw-bitmap-section (av PICS 'sprites)
          ox 0  (* 32 x) (* 32 y) 32 32)
    (list name b)))

(define (draw-sprite  dc  name  x y)
  "( dc name x y --) Draw sprite name at xy."

  (send dc draw-bitmap (av SPRITES name) x y))

(define (draw-message dc tick who message)
  (let* [(w 322) (h 86) (y (- HEIGHT h 20))
         (o (list (/ (- WIDTH 320) 2) y))]
    (using dc 'pen-black3 'brush-white)
    (send dc draw-rectangle (first o) (second o) w h)
    (send dc set-text-foreground "black")
    (for [(i (range (length message))) (line message)]
      (send dc draw-text line (+ 16 (first o)) (+ 8 y (* i 16))))

    (let [(p (v+v o (list (- w 2) (- h 30))))]
      (if (< 14 (modulo tick 28))
          (begin (using dc 'pen-none 'brush-black)                 
                 (send dc set-text-foreground "white"))
          (begin (using dc 'pen-black 'brush-white)
                 (send dc set-text-foreground "black")))
      (send dc draw-rectangle (first p) (second p) 68 23)
      (send dc draw-text "(ENTER)" (+ 4 (first p)) (+ 2 (second p)))
      (send dc set-text-foreground "black"))
    
    ;; TODO Need to draw face if specified... 
    ))



(define SPRITES (for/list [(s SPRITE-DEF)] (apply sp s)))


;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                                          ;;    ;;;;                                      
;  ;;;;; ;;;;;                              ;       ;                                      
;    ;     ;                                ;       ;                                      
;    ;     ;     ;;;;     ;; ;;;       ;;;; ;       ;        ;;;;      ;;  ;;;     ;;;; ;  
;    ;     ;    ;    ;     ;;   ;     ;    ;;       ;       ;    ;      ; ;   ;   ;    ;;  
;    ;     ;          ;    ;     ;   ;      ;       ;      ;      ;     ;;        ;        
;    ;;;;;;;          ;    ;     ;   ;      ;       ;      ;      ;     ;          ;;;;    
;    ;     ;    ;;;;;;;    ;     ;   ;      ;       ;      ;;;;;;;;     ;              ;   
;    ;     ;   ;      ;    ;     ;   ;      ;       ;      ;            ;               ;  
;    ;     ;   ;      ;    ;     ;   ;      ;       ;      ;            ;         ;     ;  
;    ;     ;   ;     ;;    ;     ;    ;    ;;       ;       ;     ;     ;         ;;   ;   
;  ;;;;; ;;;;;  ;;;;; ;;  ;;;   ;;;    ;;;; ;;  ;;;;;;;;;    ;;;;;     ;;;;;;     ; ;;;    
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          


(define (draw-bg b room)
  "(b room -- b) Compose background layer into bitmap b from current room description."

  ;; (printf "draw-bg: room: ~a~n" room) ; Debug.
  (let* [(dc (new bitmap-dc% [bitmap b]))
         (tiles  (pqv room 'tiles 'map))
         (rw (pqv room 'width 'map))    (rh (pqv room 'height 'map))
         (ox (- (/ WIDTH 2)  (/ (* TILE-X rw) 2))) (hox (/ ox 2)) ; Use when scaled
         (oy (- (/ HEIGHT 2) (/ (* TILE-Y rh) 2))) (hoy (/ oy 2))
         (y 0)
         (items (map (λ (x) (list (second x) (third x))) (pq room 'item #f)))
         ]

    (let [(backdrop (pqv room 'backdrop #f))]
      (when backdrop (send dc draw-bitmap (av PICS backdrop) 0 0)))

    (send dc scale 2 2)
    (for [(i (range (* rw rh))) (t tiles)]
      (let [(tx (* 32 (modulo i rw)))
            (ty (* 32 (floor (/ i rw))))
            (sx (* 32 (modulo t 16)))
            (sy (* 32 (floor (/ t 16))))]
        (send dc draw-bitmap-section (av PICS 'sprites)
              (+ hox tx) (+ hoy ty)
              sx sy 32 32)))
    (send dc scale 1 1)

    b))


(define (prepare-qroom rooms name)
  "( rooms name -- qr) Build qroom acceleration structure from named room."

  (let* [(room (ar rooms (or name 'default)))
         (rw (pqv room 'width 'map))    (rh (pqv room 'height 'map))
         (tiles  (pqv room 'tiles 'map))
         (ox (- (/ WIDTH 2)  (/ (* TILE-X rw) 2)))
         (oy (- (/ HEIGHT 2) (/ (* TILE-Y rh) 2)))
         ]

    (qroom name
           (stub-load-mus (av MUSICS (pqv room 'theme 'music)))
           (list rw rh)
           (list ox oy)
           tiles
           (draw-bg (make-object bitmap% WIDTH HEIGHT) room)
           (for/list [(t tiles)] (eq? -1 t))
           (for/list [(r (pq room 'trap 'map))] (last r))
           '()
           )))


(define (draw-map dc w)
  "( dc w --) Draw map overlay."

  (let* [(zodemap (av PICS 'zodemap))
         (mw (send zodemap get-width)) (mh (send zodemap get-height))
         (ox (/ (- WIDTH mw) 2)) (oy (/ (- HEIGHT mh) 2))
         (a (/ pi 2)) ;; Precompute angle so I'm not doing this div all the time.
         ]
    (send dc draw-bitmap zodemap ox oy)
    (using dc 'pen-black3 'brush-none)
    (send dc draw-rectangle ox oy  mw mh)
    (using dc 'pen-black 'brush-none)
    (send dc draw-rectangle (+ ox 8) (+ oy 8) (- mw 16) (- mh 16))

    (for [(h MAP-ZODES) (i (range (length MAP-ZODES)))]
      (if (<= i (or (av w 'map-open) 0))
          (using dc 'pen-black 'brush-none)
          (using dc 'pen-x     'brush-/))
      (draw-hex dc (first h) (second h) 41 a))

    (let* [(current (or (av w 'map-hex-c) 0))
           (p (list-ref MAP-ZODES current))]
      (when (> 6 (modulo (av w 'ticks) 12))
        (using dc 'pen-black3 'brush-b/)
        (draw-hex dc (first p) (second p) 41 a))
      (send dc draw-text
            (format "HEX: ~ax~a NAME: ~a"
                    (first p) (second p)
                    (if (< 2 (length p)) (third p) "??????"))
            (+ ox 16) (+ oy 16))
      )
    ))
    

(define (draw-world dc w)
  "( dc w --) Draw current world into dc."

  (let* [(qr (av w 'qroom))
         (o  (qroom-origin qr))
         (hp (v+v o (v*s (av w 'hero-pos) TILE-X)))]

    (send dc draw-bitmap (qroom-bg qr) 0 0)

    (draw-sprite dc
                 (if (eq? 'right (av w 'hero-face)) 'pele-r-stand 'pele-l-stand)
                 (first hp) (second hp))
  ))


(define (draw-gui w)
  "( w -- b) Compose screen display from world w."
  
  (let* ([dc (new bitmap-dc% [bitmap VIEW])])
    (using dc 'pen-black 'brush-white)
    (send dc draw-rectangle 0 0 WIDTH HEIGHT)

    (when (av w 'qroom)
      (draw-world dc w)
      (when (av w 'map) (draw-map dc w))
      (let [(m (av w 'message))]
        (when m (draw-message dc (av w 'ticks) (first m) (rest m))))
      )

    (when (av w 'grid)
      (let* [(qr (av w 'qroom))
             (o (if qr (qroom-origin qr) '(0 0)))
             (s (if qr (v*s (qroom-size qr) TILE-X) (list WIDTH HEIGHT)))]
        (using dc 'pen-red 'brush-none)
        (for [(x (range (first o) (+ 1 (first o) (first s)) TILE-X))]
          (send dc draw-line x (second o) x (+ (second o) (second s))))
        (for [(y (range (second o) (+ 1 (second o) (second s)) TILE-Y))]
          (send dc draw-line (first o) y (+ (first o) (first s)) y))))
    (when (av w 'debug)
      (send dc set-text-foreground "red")
      (send dc draw-text (format "ticks: ~a" (av w 'ticks)) 16 16)
      (send dc draw-text (format "state: ~a" w) 16 32)
      (send dc set-text-foreground "black"))
    (when (av w 'dbg-trap)
      (let* [(o (qroom-origin (av w 'qroom)))]
        (using dc 'pen-red3 'brush-none)
        (for [(t (qroom-traps (av w 'qroom)))]
          (let [(p (v+v o (v+v '(32 32) (v*s (first t) TILE-X))))]
            (draw-circle dc (first p) (second p) 20)))))
    (when (av w 'dbg-tile)
      (let* [(o (qroom-origin (av w 'qroom)))
             (p (or (av w 'dbg-tile-p) (av w 'hero-pos)))
             (x (+ (first o) (* TILE-X (first p))))
             (y (+ (second o) (* TILE-Y (second p))))
             (i (+ (first p) (* (second p) (first (qroom-size (av w 'qroom))))))]
        (using dc 'pen-red3 'brush-none)
        (send dc draw-rectangle x y TILE-X TILE-Y)
        (send dc set-text-foreground "red")
        (send dc draw-text (format "~ax~a" (first p) (second p))  (+ 4 x) (+ 4 y))
        (send dc draw-text (format "(~a)" i) (+ 4 x) (+ 20 y))
        (send dc draw-text (format "t#~a" (list-ref (qroom-tiles (av w 'qroom)) i)) (+ 4 x) (+ 36 y))
        (send dc set-text-foreground "black")))

    VIEW))


;                                                                    
;                                                                    
;                                                                    
;                                        ;                           
;                                        ;                   ;       
;      ;;; ;                                                 ;       
;     ;   ;;                                                 ;       
;    ;     ;      ;;;; ;   ;;  ;;;    ;;;;      ;; ;;;;    ;;;;;;;;  
;    ;           ;    ;;    ; ;   ;      ;       ;;    ;     ;       
;    ;;         ;      ;    ;;           ;       ;      ;    ;       
;      ;;;      ;           ;            ;       ;      ;    ;       
;         ;;    ;           ;            ;       ;      ;    ;       
;          ;    ;           ;            ;       ;      ;    ;       
;    ;     ;    ;           ;            ;       ;      ;    ;       
;    ;;   ;      ;     ;    ;            ;       ;;    ;     ;    ;; 
;    ; ;;;        ;;;;;    ;;;;;;    ;;;;;;;;;   ; ;;;;       ;;;;   
;                                                ;                   
;                                                ;                   
;                                                ;                   
;                                               ;;;                  
;                                                                    


(define (queue-event kind arg w)
  "( kind arg w -- w) Record event for later processing."

  (sv w 'event (list kind arg)))

(define (room-jump w name pos face)
  "( w name pos face -- w) Jump to new room."

  ; Do room enter event.
  (queue-event 'enter name (sv (sv (sv (sv w 'room name) ; Do I really need the 'room parm anymore?
                  'qroom
                  (let [(q (prepare-qroom ROOMS (or name 'default)))]
                    (when (qroom-music q)
                      (stub-clear-error!)
                      (stub-fade-out-music 500) ; This is blocking! Grrr...
                      (unless (stub-play-music (qroom-music q) -1)
                        (eprintf "Can't play music: ~a~n" (stub-get-error)))
                      )
                    q))
              'hero-pos pos)
          'hero-face face))) ; Queue up event to be processed.


(define (next w)
  "( w -- w) Advance script PC."

  (let [(f (rest (av w 'script)))]
    (sv w 'script (if (empty? f) #f f))))

(define (step w)
  "( w -- w) Execute one script form."

  (let [(action (first (av w 'script)))]
    (when (av w 'debug) (printf "--action: ~a~n" action))
    (case (first action)
      [(end)   (sv w 'script #f)]
      [(sfx)   (let [(s (av SOUNDS (second action)))]
                 (when s (stub-play-channel -1 s 0))
                 (next w))]
      [(theme) (next w)] ; Nop for now.
      [(tell)  (next (sv w 'message (append '(#f) (rest action))))] ; Narrator sez.
      [(say)   (next (sv w 'message (rest action)))] ; Character sez.
      [(goal)  (next (sv w 'goal (rest action)))]
      [(move)  (next (sv (sv w 'hero-pos (second action))
                        'hero-face (third action)))]
      [(jump)  (next (room-jump w (second action) (third action) (fourth action)))]
      [(set!)  (next (sv w (second action)
                         (if (= 3 (length action)) (third action) #t)))]
      [(is?)   (if (av w (second action)) ; Flow control!
                   (sv w 'script (rest (rest action)))
                   (next w))]
      [else    (eprintf "Unrecognized keyword: ~a~n" action)
               (sv w 'script #f)])))
      
(define (proc w)
  "( w -- w) Run script."
  
  (if (av w 'message) w
      (if (not (av w 'script)) w
          (proc (step w)))))

(define (collect-events w)
  "( w -- w) If any events collect and update script field. Nukes existing script. As per prophecy."

  ;; (printf "-- collect-events: w: ~a~n" w) ; Debug.
  (let [(e (av w 'event))]
    (sv (cond
          [(not e) w]
          [(eq? 'enter (first e))
           (let [(s (pqv (ar ROOMS (second e)) 'event 'enter))]
             (if s  (sv w 'script s)  w))]
          [(eq? 'hit (first e))
           (let* [(found (filter (λ (x) (equal? (first x) (second e)))
                                 (qroom-traps (av w 'qroom))))]
             (if (empty? found)  w  (sv w 'script (rest (first found)))))]
          [else w]) ;; Unhandled events.
        'event #f)))
             
(define (ticking w)
  "( w -- w) Process world updates."

  (sv (proc (collect-events w))   ; Run any pending scripts.
      'ticks (+ (av w 'ticks) 1)) ; Update counter.
  )


;                                              
;                                              
;                                              
;                             ;                
;                             ;                
;  ;;;     ;;;                                 
;   ;;     ;;                                  
;   ; ;   ; ;    ;;;;      ;;;;      ;; ;;;    
;   ; ;   ; ;   ;    ;        ;       ;;   ;   
;   ; ;   ; ;         ;       ;       ;     ;  
;   ;  ; ;  ;         ;       ;       ;     ;  
;   ;  ; ;  ;   ;;;;;;;       ;       ;     ;  
;   ;   ;   ;  ;      ;       ;       ;     ;  
;   ;       ;  ;      ;       ;       ;     ;  
;   ;       ;  ;     ;;       ;       ;     ;  
;  ;;;     ;;;  ;;;;; ;;  ;;;;;;;;;  ;;;   ;;; 
;                                              
;                                              
;                                              
;                                              
;                                              

(define sdlm-ready (stub-set-main-ready!))
(stub-sdl2-init! '(audio))
(define sdlm-formats (stub-init! '(ogg mp3)))
(stub-clear-error!)
(unless (eq? 0 (stub-open-audio! 22050 stub-default-format stub-default-channels 512)) ; Recommended defaults.
  (println (format "Error: Can't open audio: ~a" (stub-get-error))))

;; Moved this down here so audio device will be open to load.

(when (not (equal? "" (stub-get-error)))
  (eprintf "Error loading sounds: ~a~n" (stub-get-error)))

(define final
  (big-bang '((ticks 0)
              (script ((set! map-open 0)
                       ;; (jump default (1 1) right) ; Testing.
                       (jump Title (4 4) right)
                       )))
    (name (av APP 'name))
    (to-draw draw-gui)
    (on-tick ticking)
    (on-key keying)
    ;; (state #t) ; State monitor. Comment out if you don't need it.
    ))

(stub-halt-music)
(stub-close-audio!)