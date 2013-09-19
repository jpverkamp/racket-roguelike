#lang racket

(provide 
 game-screen%)

(require 
 racket/draw
 noise
 thing
 "point.rkt"
 "screen.rkt"
 "entities.rkt"
 "world.rkt")

(define gray-background (make-object color% 32 32 32))

(define game-screen%
  (class screen%
    ; Store the world, which contains player/map/etc
    (define world (new world%))
    
    ; Process keyboard events
    (define game-over #f)
    (define/override (update key-event)
      (cond
        [game-over (new game-screen%)]
        [else
         (define player (send world get-player))
         
         ; NOTE: Y axis is top down, X axis is left to right
         
         ; Try to move the player
         (case (send key-event get-key-code)
           [(numpad1)           (send world try-move player (+ (pt  1 -1) (thing-get player 'location)))]
           [(numpad2 #\s down)  (send world try-move player (+ (pt  0 -1) (thing-get player 'location)))]
           [(numpad3)           (send world try-move player (+ (pt -1 -1) (thing-get player 'location)))]
           [(numpad4 #\a left)  (send world try-move player (+ (pt  1  0) (thing-get player 'location)))]
           [(numpad6 #\d right) (send world try-move player (+ (pt -1  0) (thing-get player 'location)))]
           [(numpad7)           (send world try-move player (+ (pt  1  1) (thing-get player 'location)))]
           [(numpad8 #\w up)    (send world try-move player (+ (pt  0  1) (thing-get player 'location)))]
           [(numpad9)           (send world try-move player (+ (pt -1  1) (thing-get player 'location)))])
         
         ; Update npcs and lighting
         (send world update)
         
         ; Check if the player is dead
         ; If so, tell the player they lost
         ; Otherwise, keep on the current screen
         (when (<= (thing-get player 'health) 0)
           (send world log "You lose!")
           (send world log "Press any key to continue.")
           (set! game-over #t))
         
         this]))
    
    ; Draw the game itself.
    (define/override (draw canvas)
      (define player (send world get-player))
      (send canvas clear)
      
      ; Update lighting
      (send world update-lighting)
      
      ; Draw the tiles around the player
      (for* ([xi (in-range (send canvas get-width-in-characters))]
             [yi (in-range (send canvas get-height-in-characters))])
        (define x/y (recenter canvas (- (thing-get player 'location) (pt xi yi))))
        (define tile (send world tile-at (pt-x x/y) (pt-y x/y)))
        (define lighting (thing-get tile 'lighting 'lit))
        (cond
          ; Draw nothing on dark tiles
          [(eq? lighting 'dark)
           (void)]
          ; If it's in fog, draw with a gray background
          [(eq? lighting 'fog)
           (send canvas write
                 (thing-get tile 'character)
                 xi
                 yi
                 (thing-get tile 'color)
                 gray-background)]
          ; If it's lit but there are no items, draw the tile
          [(null? (thing-get tile 'items '()))
           (send canvas write
                 (thing-get tile 'character)
                 xi
                 yi
                 (thing-get tile 'color))]
          ; If it's lit and there are items, draw the item
          [else
           (send canvas write
                 (thing-get (car (thing-get tile 'items)) 'character)
                 xi
                 yi
                 (thing-get (car (thing-get tile 'items)) 'color))]))
      
      ; Draw the player centered on the screen
      (let ([draw-@ (recenter canvas (pt 0 0))])
        (send canvas write #\@ (pt-x draw-@) (pt-y draw-@)))
      
      ; Draw the npcs
      (send world draw-npcs canvas)
      
      ; Show current player statistics
      (for ([i (in-naturals)]
            [key (in-list '(attack defense health))])
        (send canvas write-string
              (format "~a: ~a" key (thing-get player key))
              1 (+ i 1)
              "green"))
      
      ; Show current player inventory
      (for ([i (in-naturals)]
            [item (in-list (thing-get player 'inventory))])
        (send canvas write-string
              (format "~a: ~a" 
                      (thing-get item 'category)
                      (if (thing-get item 'stackable)
                          (thing-get item 'quantity)
                          (thing-get item 'name)))
              1 (+ i 5)
              (thing-get item 'color)))
      
      ; Draw recent log messages
      (for ([i (in-naturals)]
            [msg (in-list (send world get-log 3))])
        (send canvas write-string
              msg
              1 (- (send canvas get-height-in-characters) i 2)
              "green")))
    
    (super-new)))