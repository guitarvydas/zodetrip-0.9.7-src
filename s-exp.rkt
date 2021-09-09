#lang peg
;; from https://github.com/rain-1/racket-peg/blob/master/peg-src/s-exp.peg
(require "peg-result.rkt");

(define char-table
  '(("null" . #\null)
    ("nul" . #\null)
    ("backspace" . #\backspace)
    ("tab" . #\tab)
    ("newline" . #\newline)
    ("vtab" . #\vtab)
    ("page" . #\page)
    ("return" . #\return)
    ("space" . #\space)
    ("rubout" . #\rubout)));

(define (symbol->keyword sym)
 (string->keyword (symbol->string sym)));

_ < [ \t\n]*;

s-exp <-
  list / box-list /
  quote / quasiquote / unquote /
  syntax-quote / syntax-quasiquote / syntax-unquote /
  boolean / number / identifier / dotdotdot / keyword / character / string;

list <- '(' _ lst:(s-exp _)*
        (dotted-pair:'.' _ back:s-exp)?
        ')'
     -> (if dotted-pair
            (append lst back)
            lst);

box-list <-
        '[' _ lst:(s-exp _)*
        (dotted-pair:'.' _ back:s-exp)?
        ']'
     -> (if dotted-pair
            (append lst back)
            lst);

quote             <- '\''  _ s:s-exp -> (list 'quote s);
quasiquote        <- '`'   _ s:s-exp -> (list 'quasiquote s);
unquote           <- ','   _ s:s-exp -> (list 'unquote s);
syntax-quote      <- '#\'' _ s:s-exp -> (list 'syntax s);
syntax-quasiquote <- '#`'  _ s:s-exp -> (list 'quasisyntax s);
syntax-unquote    <- '#,'  _ s:s-exp -> (list 'unsyntax s);

boolean    <- x:'#t' / x:'#f'               -> (equal? "#t" x);
identifier <- s:[^. \t\n()\[\]{}",'`;#|\\]+ -> (string->symbol s);
dotdotdot  <- '...'                         -> '...;
keyword    <- '#:' s:identifier             -> (symbol->keyword s);
number     <- floating-point;
string     <- ["] s:([^"\\] / escaped-char)* ["] -> s;

escaped-char <- escaped-newline / escaped-tab / ~[\\] .;
escaped-newline <- [\\] 'n' -> (peg-result "\n");
escaped-tab     <- [\\] 't' -> (peg-result "\t");

character <- ~'#\\' v:code -> v;
alphabetic <- v:[a-zA-Z] -> (string-ref v 0);
code <- named-char / alphabetic-code / digit;
named-char <- nm:('null' / 'nul' / 'backspace' / 'tab' / 'newline' / 'tab' / 'vtab' / 'page' / 'return' / 'space' / 'rubout') !alphabetic
  -> (cdr (assoc nm char-table));
alphabetic-code <- v:alphabetic ! alphabetic -> v;
digit <- v:[0-9] -> (string-ref v 0);


signal <- '-' / '+';
integer-part <- [0-9]+;
fractional-part <- [0-9]+;
scientific-notation <- ('e' / 'E') integer-part;
floating-point <- value:(signal? integer-part ('.' fractional-part)? (scientific-notation)?) -> (string->number value);