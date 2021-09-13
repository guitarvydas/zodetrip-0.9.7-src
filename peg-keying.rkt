#lang peg
(require "../racket-peg/s-exp.rkt");

gEnter <- 'enter' -> "\r";
gLeft <- 'left' -> "left";
gRight <- 'right' -> "right";
gUp <- 'up' -> "up";
gDown <- 'down' -> "down";

gestureKeystroke <- gEnter / gLeft / gRight / gUp / gDown;

gesture <- gkey:gestureKeystroke exp:s-exp;

keymap <- keyvar:s-exp gestures:gesture+;
