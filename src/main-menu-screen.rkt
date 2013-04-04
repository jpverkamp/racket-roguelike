#lang racket

(provide
 main-menu-screen%)

(require
 "screen.rkt"
 "game-screen.rkt")

(define main-menu-screen%
  (class screen%
    ; Always turn control over the game screen
    (define/override (update key-event)
      (new game-screen%))
    
    ; Just draw a basic menu
    (define/override (draw canvas)
      (send canvas clear)
      (send canvas write-center "Racket Roguelike" 10)
      (send canvas write-center "Press any key to continue" 12))
    
    (super-new)))