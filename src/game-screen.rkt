#lang racket

(provide 
 game-screen%)

(require 
 "screen.rkt"
 "noise/noise.rkt")

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
    (define player (pt -11 -9))
    
    ; Get the contents of a given point, caching for future use
    ; Hash on (x y) => char
    (define caves (make-hash))
    (define (get-tile x y)
      (unless (hash-has-key? caves (list x y))
        (hash-set! caves (list x y)
          (let ()
            (define wall?  (> (simplex (* 0.1 x) (* 0.1 y) 0)         0.0))
            (define water? (> (simplex (* 0.1 x) 0         (* 0.1 y)) 0.5))
            (define tree?  (> (simplex 0         (* 0.1 x) (* 0.1 y)) 0.5))
            (cond
              [wall?  'wall]
              [water? 'water]
              [tree?  'tree]
              [else   'empty]))))
      (hash-ref caves (list x y)))
    
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
      
      ; Draw some caverns around the player
      (for* ([xi (in-range (send canvas get-width-in-characters))]
             [yi (in-range (send canvas get-height-in-characters))])
        (define x/y (recenter canvas (+ (pt xi yi) player)))
        (case (get-tile (pt-x x/y) (pt-y x/y))
          [(wall) (send canvas write #\# xi yi)]
          [(water) (send canvas write #\space xi yi "blue" "blue")]
          [(tree) (send canvas write #\u0005 xi yi "green")]))
      
      ; Draw the player centered on the screen
      (let ([player (recenter canvas (pt 0 0))])
        (send canvas write #\@ (pt-x player) (pt-y player)))
      
      ; Debug: Show the player location
      (send canvas write-string
            (format "~a, ~a" (pt-x player) (pt-y player))
            1 1
            "green"))
    
    (super-new)))