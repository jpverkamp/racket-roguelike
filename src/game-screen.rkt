#lang racket

(provide 
 game-screen%)

(require 
 "point.rkt"
 "screen.rkt"
 "noise/noise.rkt"
 "thing/thing.rkt"
 "entities.rkt"
 "world.rkt")

(define game-screen%
  (class screen%
    ; Store the world, which contains player/map/etc
    (define world (new world%))
    (define player (make-thing entity))
    
    ; Process keyboard events
    (define/override (update key-event)
      ; NOTE: Y axis is top down, X axis is left to right
      
      ; Find where we are attempting to go
      (define target (thing-get player 'location))
      (case (send key-event get-key-code)
        [(numpad8 #\w up)    (set! target (+ (pt  0  1) (thing-get player 'location)))]
        [(numpad4 #\a left)  (set! target (+ (pt  1  0) (thing-get player 'location)))]
        [(numpad2 #\s down)  (set! target (+ (pt  0 -1) (thing-get player 'location)))]
        [(numpad6 #\d right) (set! target (+ (pt -1  0) (thing-get player 'location)))])
      
      ; Only move if it's open
      (when (eq? 'empty (send world get-tile (pt-x target) (pt-y target)))
        (thing-set! player 'location target))
      
      ; Keep the state
      this)
    
    ; Draw the game itself.
    (define/override (draw canvas)
      (send canvas clear)
      
      ; Draw some caverns around the player
      (for* ([xi (in-range (send canvas get-width-in-characters))]
             [yi (in-range (send canvas get-height-in-characters))])
        (define x/y (recenter canvas (- (thing-get player 'location) (pt xi yi))))
        (case (send world get-tile (pt-x x/y) (pt-y x/y))
          [(wall) (send canvas write #\# xi yi)]
          [(water) (send canvas write #\space xi yi "blue" "blue")]
          [(tree) (send canvas write #\u0005 xi yi "green")]))
      
      ; Draw the player centered on the screen
      (let ([draw-@ (recenter canvas (pt 0 0))])
        (send canvas write #\@ (pt-x draw-@) (pt-y draw-@)))
      
      ; Debug: Show the player location
      (send canvas write-string
            (format "~a, ~a" 
                    (pt-x (thing-get player 'location)) 
                    (pt-y (thing-get player 'location)))
            1 1
            "green"))
    
    (super-new)))