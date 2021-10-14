#lang peg
(provide peg-keying-1);
(require "../racket-peg/s-exp.rkt");
(require "sv.rkt" "av.rkt" "move.rkt");
(require "./keymapstr.rkt");
(define (compile-snippet e)
  (println (format "e=~a" e))
  (println (format "(syntax? e)->~a, (pair? e)->~a"
                   (syntax? e) (pair? e)))
  (let ((p (eval e)))
    (println (format "in compile-snippet p syntax? ~a, p pair? ~a, p proc? ~a"
                     (syntax? p) (pair? p) (procedure? p)))
    p));
(define (xxpeg-keying-1 w k)
  (let ((expr (peg keymap keymapstr)))
    (let ((fn (compile-snippet expr)))
      (fn w k))));
(define (peg-keying-1 w k)
  (let ((fn (peg keymap keymapstr)))
      (fn w k)));

gEnter <- 'enter' -> "\"\\r\"";
gLeft <- 'left' -> "\"left\"";
gRight <- 'right' -> "\"right\"";
gUp <- 'up' -> "\"up\"";
gDown <- 'down' -> "\"down\"";

gestureKeystroke <- gEnter / gLeft / gRight / gUp / gDown;

gesture <- _ gkey:gestureKeystroke _ ':' _ exp:s-exp _ -> (list (list gkey) exp);
gestures <- g:gesture+ -> g;
xkeymap <- gs:gestures -> #'(lambda (w k) (println "k") (println k) (cons 'case (cons 'k (append gs '((else w))))));
keymap <- gs:gestures -> (let ((e (lambda (w k) (println "hello")))
                               (e2 (lambda () (println gs))))
                           (println e)
                           (println e2)
                           (let ((p2 (e2)))
                             (println p2)
                             (let ((p (eval e)))
                               (println p)
                               p)));
