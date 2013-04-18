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
    
    ; Process keyboard events
    (define/override (update key-event)
      (define player (send world get-player))
      
      ; NOTE: Y axis is top down, X axis is left to right
      
      ; Try to move the player
      (case (send key-event get-key-code)
        [(numpad8 #\w up)    (send world try-move player (+ (pt  0  1) (thing-get player 'location)))]
        [(numpad4 #\a left)  (send world try-move player (+ (pt  1  0) (thing-get player 'location)))]
        [(numpad2 #\s down)  (send world try-move player (+ (pt  0 -1) (thing-get player 'location)))]
        [(numpad6 #\d right) (send world try-move player (+ (pt -1  0) (thing-get player 'location)))])
      
      ; Update npcs
      (send world update-npcs)
      
      ; Keep the state
      this)
    
    ; Draw the game itself.
    (define/override (draw canvas)
      (define player (send world get-player))
      (send canvas clear)
      
      ; Draw some caverns around the player
      (for* ([xi (in-range (send canvas get-width-in-characters))]
             [yi (in-range (send canvas get-height-in-characters))])
        (define x/y (recenter canvas (- (thing-get player 'location) (pt xi yi))))
        (define tile (send world get-tile (pt-x x/y) (pt-y x/y)))
        (send canvas write
              (thing-get tile 'character)
              xi
              yi
              (thing-get tile 'color)))
      
      ; Draw the player centered on the screen
      (let ([draw-@ (recenter canvas (pt 0 0))])
        (send canvas write #\@ (pt-x draw-@) (pt-y draw-@)))
      
      ; Draw the npcs
      (send world draw-npcs canvas)
      
      ; Show current player statistics
      (for ([i (in-naturals)]
            [key (in-list '(health defense attack))])
        (send canvas write-string
              (format "~a: ~a" key (thing-get player key))
              1 (- (send canvas get-height-in-characters) i 2)
              "green"))
      
      ; Debug: Show the player location
      (send canvas write-string
            (format "~a, ~a" 
                    (pt-x (thing-get player 'location)) 
                    (pt-y (thing-get player 'location)))
            1 1
            "green"))
    
    (super-new)))