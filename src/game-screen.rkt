#lang racket

(provide 
 game-screen%)

(require 
 "screen.rkt")

; API to use imaginary numbers as points
(define pt make-rectangular)
(define pt-x real-part)
(define pt-y imag-part)

; Convert real coordinates to canvas coordinates
(define (recenter canvas orig)
  (+ orig (pt (quotient (send canvas get-width-in-characters) 2)
              (quotient (send canvas get-height-in-characters) 2))))

(define game-screen%
  (class screen%
    ; Store the player's state
    ; Use an imaginary number for a point
    (define player (pt 0 0))
    
    ; Process keyboard events
    (define/override (update key-event)
      ; Move the player
      ; NOTE: Y axis is top down, X axis is left to right
      (case (send key-event get-key-code)
        [(numpad8 #\w up)    (set! player (+ (pt  0 -1) player))]
        [(numpad4 #\a left)  (set! player (+ (pt -1  0) player))]
        [(numpad2 #\s down)  (set! player (+ (pt  0  1) player))]
        [(numpad6 #\d right) (set! player (+ (pt  1  0) player))])
      
      ; Keep the state
      this)
    
    ; Draw the game itself.
    (define/override (draw canvas)
      (send canvas clear)
      
      ; Draw the player
      ; 0x0 is the center point of the canvas
      (let ([player (recenter canvas player)])
        (send canvas write #\@ (pt-x player) (pt-y player))))
    
    (super-new)))