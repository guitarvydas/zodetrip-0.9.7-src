#lang racket/gui
(provide qroom qroom-name qroom-music qroom-size qroom-origin qroom-tiles qroom-bg qroom-floor
         qroom-traps qroom-mobs)
(struct qroom ; "Quick" room. Acceleration structure for drawing and movement.
  (name
   music
   size       ; Map size in tiles.
   origin     ; Map draw origin in pixels (for mobs).
   tiles      ; Tile layout.
   bg         ; bg is bitmap layer.
   floor      ; Places that can be walked on.
   traps      ; Do things if locations are stepped on.
   mobs))     ; Anything not a static tile, particularly with a script.
