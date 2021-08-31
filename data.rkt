#lang racket

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

(require racket/draw)

(provide PAINTBOX SPRITE-DEF MAP-ZODES)

(define PAINTBOX
  `((pen-none   ,(new pen% [style 'transparent]))
    (pen-white3 ,(new pen% [color "white"] [width 3]))
    (pen-black  ,(new pen% [color (make-object color% "black")] [width 1]))
    (pen-black3 ,(new pen% [color (make-object color% "black")] [width 3])) 
    (pen-red    ,(new pen% [color "red"] [width 1]))
    (pen-red3   ,(new pen% [color "red"] [width 3]))
    (pen-x      ,(new pen% [color "white"] [style 'xor]))
    (pen-x3     ,(new pen% [color "white"] [style 'xor] [width 3]))
    
    (brush-none   ,(new brush% [style 'transparent]))
    (brush-black  ,(new brush% [color (make-object color% "black")]))
    (brush-white  ,(new brush% [style 'solid] [color (make-object color% "white")]))
    (brush-red    ,(new brush% [style 'solid] [color (make-object color% "red")]))
    (brush-yellow ,(new brush% [style 'solid] [color "yellow"]))
    (brush-teal   ,(make-brush #:color "teal"))
    (brush-/      ,(new brush% [style 'fdiagonal-hatch]))
    (brush-b/     ,(new brush% [style 'bdiagonal-hatch]))
    (brush-x      ,(new brush% [style 'crossdiag-hatch]))

    (font-keycap  ,(make-font #:size 24  #:family 'roman  #:weight 'bold))
    (font-note    ,(make-font #:size 10  #:family 'swiss))
    (font-name    ,(make-font #:size 16  #:family 'swiss  #:weight 'bold))
    (font-text    ,(make-font #:size 16  #:family 'swiss))
    ))

(define SPRITE-DEF
  '((pele-l-stand 0 1 #t)
    (pele-l-walk  1 1 #t)
    (pele-r-stand 0 1 #f)
    (pele-r-walk  1 1 #f)))


(define MAP-ZODES
  '((605 216 CHORAR)
    (416 253 STEAMLAND)
    (542 323 MESO)
    (354 359 KYBER)
    (480 288)
    (480 359)
    (542 252) 
    (606 287)
    (667 252)
    (732 287)
    (732 359)
    (794 323)
    (416 323)
    (292 323)
    (292 250)
    (292 178)
    (292 395)
    (230 431)
    (230 359)
    (354 288)
    (354 214)
    (354 143)
    (416 179)
    ))
