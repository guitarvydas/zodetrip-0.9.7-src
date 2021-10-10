#lang racket
(provide switch)

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