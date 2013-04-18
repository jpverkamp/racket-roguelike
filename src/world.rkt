#lang racket

(provide world%)

(require 
 "point.rkt"
 "entities.rkt"
 "noise/noise.rkt"
 "thing/thing.rkt")

; Define tile types
(define-thing tile
  [walkable #f]
  [character #\space]
  [color "black"])

(define-thing empty tile
  [walkable #t])

(define-thing wall tile
  [character #\#]
  [color "white"])

(define-thing water tile
  [character #\u00db]
  [color "blue"])

(define-thing tree tile
  [character #\u0005]
  [color "green"])

(define world%
  (class object%
    ; Store the player
    (define player (make-thing entity))
    (define/public (get-player) player)
    
    ; Get the contents of a given point, caching for future use
    ; Hash on (x y) => char
    (define tiles (make-hash))
    (define/public (get-tile x y)
      ; If the tile doesn't already exist, generate it
      (unless (hash-has-key? tiles (list x y))
        ; Generate a random tile
        (define new-tile
          (let ()
            (define wall?  (> (simplex (* 0.1 x) (* 0.1 y) 0)         0.0))
            (define water? (> (simplex (* 0.1 x) 0         (* 0.1 y)) 0.5))
            (define tree?  (> (simplex 0         (* 0.1 x) (* 0.1 y)) 0.5))
            (cond
              [wall?  wall]
              [water? water]
              [tree?  tree]
              [else   empty])))
        (hash-set! tiles (list x y) new-tile)

        
        ; Sometimes, generate a new enemy
        ; Only if the new tile is walkable
        (when (and (thing-get new-tile 'walkable)
                   (< (random 100) 1))
          (define new-thing 
            (make-thing 
             ; Base it off a randomly chosen enemy
             (vector-ref random-enemies 
                         (random (vector-length random-enemies)))
             ; This is it's location
             [location (pt x y)]))
          
          ; Store it in the npc list
          (set! npcs (cons new-thing npcs))))
        
      ; Return the tile (newly generated or not)
      (hash-ref tiles (list x y)))
    
    ; Store a list of non-player entities
    (define npcs '())
    
    (define/public (update-npcs)
      (for ([npc (in-list npcs)])
        (thing-call npc 'act npc this)))
    
    (define/public (draw-npcs canvas)
      (for ([npc (in-list npcs)])
        (define x/y (recenter canvas (- (thing-get player 'location)
                                        (thing-get npc 'location))))
        (when (and (<= 0 (pt-x x/y) (sub1 (send canvas get-width-in-characters)))
                   (<= 0 (pt-y x/y) (sub1 (send canvas get-height-in-characters))))
          (send canvas write 
                (thing-get npc 'character)
                (pt-x x/y) 
                (pt-y x/y)
                (thing-get npc 'color)))))
    
    (super-new)))