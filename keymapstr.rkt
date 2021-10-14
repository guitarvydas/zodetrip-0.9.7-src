#lang racket
(provide keymapstr)
(define xxkeymapstr #<<%%%
 enter : (let () (println "\"peg-1 enter\"") (sv w 'message #f))
 right : (let () (println "\"peg-1 right\"") (world-move w 1 0))
 left  : (let () (println "\"peg-1 left\"") (world-move w -1 0))
 up    : (let () (println "\"peg-1 up\"") (world-move w 0 -1))
 down  : (let () (println "\"peg-1 down\"") (world-move w 0 1))
%%%
  )
(define keymapstr #<<%%%
 enter : (sv w 'message #f)
%%%
  )
(define xkeymapstr #<<%%%
enter : a
right : b
%%%
)
