#lang peg

(require "s-exp.rkt");

kspace <- 'space' -> " ";
kreturn <- 'return' -> (string #\r);
km <- 'm' -> "m";
kF8 <- 'f8' -> "f8";
kF9 <- 'f9' -> "f9";
kF10 <- 'f10' -> "f10";
kF11 <- 'f11' -> "f11";
kF12 <- 'f12' -> "f12";
kRIGHT <- 'right' -> "right";
kLEFT <- 'left' -> "left";
kUP <- 'up' -> "up";
kDOWN <- 'down' -> "down";
kWHEEL-UP <- 'wheel-up' -> "wheel-up";
kWHEEL-DOWN <- 'wheel-down' -> "wheel-down";

_ <- [ \n]*;

key <- kspace;

pkeying <- keys:keystroke+ -> [list 'case 'k keys];
keystroke <- c:kclause _ ':' _ e:s-exp -> [list c e];
kclause <- v:(kspace / kreturn / km / kF8 / kF9 / kF10 / kF11 / kF12 / kRIGHT
                     / kLEFT / kUP / kDOWN / kWHEEL-UP / kWHEEL-DOWN) -> v;
