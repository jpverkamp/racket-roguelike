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
    (define player 
      (make-thing entity 
        [attack 10]
        [defense 10]
        [health 100]))
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
    
    ; Try to move an entity to a given location
    (define/public (try-move entity target)
      (define tile (send this get-tile (pt-x target) (pt-y target)))
      (define others
        (filter
         ; Only get ones at the target location that aren't me
         (lambda (thing) (and (not (eqv? thing entity))
                              (= (thing-get thing 'location) target)))
         ; Include the player and all npcs
         (cons player npcs)))
      
      (cond
        ; If it's not walkable, do nothing
        [(not (thing-get tile 'walkable))
         (void)]
        ; If it's walkable and not occupied, update the location
        [(null? others)
         (thing-set! entity 'location target)]
        ; If it's walkable and occupied, attack the occupant and don't move
        ; damage = max(0, rand(min(1, attack)) - rand(min(1, defense)))
        [else
         (for ([other (in-list others)])
           (printf "~s is fighting ~s\n"
                   (thing-get entity 'location)
                   (thing-get other 'location))
           
           ; I attack them
           (define other-damaged
             (max 0 (- (random (min 1 (thing-get entity 'attack)))
                       (random (min 1 (thing-get other 'defense))))))
           (thing-set! other 'health (- (thing-get other 'health) other-damaged))
           
           ; They attack me
           (define entity-damaged
             (max 0 (- (random (min 1 (thing-get other 'attack)))
                       (random (min 1 (thing-get entity 'defense))))))
           (thing-set! entity 'health (- (thing-get entity 'health) entity-damaged)))]))
    
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