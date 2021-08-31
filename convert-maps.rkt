#lang racket

; This program will read %.ldtk files and produce a layout
; specification for Zodetrip rooms.

(require json)


(define (av data key)
  "( data key -- value) Get value from association list."

  (let ([found (assoc key data)])
    (if found (last found) #f)))


(define (expand w h spec)
  "( w h spec -- tiles) Expand gridTiles to layout."

  (let* [(sets (for/list [(t spec)]
                 (list (first (hash-ref t 'd)) (hash-ref t 't))))]
    (for/list [(i (range (* w h)))] (or (av sets i) -1))))
  

(define (level l)
  "( l -- room) Process level (room) data."

  (let* [(li   (first (hash-ref l 'layerInstances)))
         (w    (hash-ref li '__cWid))
         (h    (hash-ref li '__cHei))]
    (list (string->symbol (hash-ref l 'identifier))
          (list 'width  'map w)
          (list 'height 'map h)
          (list 'tiles  'map (expand w h (hash-ref li 'gridTiles))))))
  

(define x (read-json (open-input-file "zodetrip.ldtk")))

(for/list [(l (hash-ref x 'levels))] (level l))
