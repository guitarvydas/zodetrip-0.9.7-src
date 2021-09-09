#lang racket/gui
(require peg "pkeySimple.rkt")
(peg pkeying "
f9 : (symbol->string 'HELLO)
f10 : (symbol->string 'goodbye)
")