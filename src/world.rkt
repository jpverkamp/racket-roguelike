#lang racket

(provide world%)

(require 
 thing
 "point.rkt"
 "entities.rkt"
 "items.rkt"
 "levels.rkt")

; Choose a random element from a vector
(define (vector-choose-random v)
  (vector-ref v (random (vector-length v))))

; Choose a random element from a vector with a falling likelihood
; TODO: Fix this
(define (vector-choose-biased v)
  (vector-choose-random v))

(define world%
  (class object%
    ; Store the player
    (define player 
      (make-thing entity 
        [name "player"]
        [attack 10]
        [defense 10]
        [health 100]))
    (define/public (get-player) player)

    ; Log messages to display to the player
    (define log-messages '())

    (define/public (log msg)
      (set! log-messages (cons msg log-messages)))
    
    (define/public (get-log [count 1])
      (let loop ([i 0] [msgs log-messages])
        (cond
          [(= i count) '()]
          [(null? msgs) (cons "" (loop (+ i 1) msgs))]
          [else
           (cons (car msgs)
                 (loop (+ i 1) (cdr msgs)))])))
    
    ; One entity attacks another
    (define/public (attack entity other)
      ; Do the damage
      (define damage
        (max 0 (- (random (max 1 (thing-get entity 'attack)))
                  (random (max 1 (thing-get other 'defense))))))
      (thing-set! other 'health (- (thing-get other 'health) damage))
      
      ; Log a message
      (send this log
            (format "~a attacked ~a, did ~a damage"
                    (thing-get entity 'name)
                    (thing-get other 'name)
                    damage)))
    
    ; Wrap the tile referencing
    (define/public (tile-at x y) 
      (get-tile x y))
      
    ; Try to move an entity to a given location
    (define/public (try-move entity target)
      (define tile (get-tile (pt-x target) (pt-y target)))
      (define others
        (filter
         ; Only get ones at the target location that aren't me
         (lambda (thing) (and (not (eqv? thing entity))
                              (= (thing-get thing 'location) target)))
         ; Include the player and all npcs
         (cons player (get-npcs))))
       
      ; Otherwise, deal with moving
      (cond
        ; If it's not walkable, do nothing
        [(not (thing-get tile 'walkable))
         (void)]
        ; If it's walkable and not occupied, update the location
        ; Also, pick up any items there, exchanging if types match
        ; Also also, if there's an on-enter method, call it
        [(null? others)
         (thing-set! entity 'location target)
         
         ; Pick up an item from the ground
         (define (pick-up item)
           (send this log (format "~a picked up ~a" (thing-get entity 'name) (thing-get item 'name)))
           (thing-set! entity 'inventory (cons item (thing-get entity 'inventory)))
           (thing-set! tile 'items (remove item (thing-get tile 'items)))
           (thing-call item 'on-pick-up item entity this))
         
         ; Drop an item from the inventory
         (define (drop item)
           (send this log (format "~a dropped ~a" (thing-get entity 'name) (thing-get item 'name)))
           (thing-set! entity 'inventory (remove item (thing-get entity 'inventory)))
           (thing-set! tile 'items (cons item (thing-get tile 'items)))
           (thing-call item 'on-drop item entity this))
         
         ; Consume a consumable item from the ground
         (define (consume item)
           (send this log (format "~a consumed ~a" (thing-get entity 'name) (thing-get item 'name)))
           (thing-set! tile 'items (remove item (thing-get tile 'items)))
           (thing-call item 'on-pick-up item entity this))
         
         ; Add a stackable item from the ground
         (define (stack item)
           (send this log (format "~a picked up ~a worth ~a ~a" 
                                  (thing-get entity 'name) 
                                  (thing-get item 'name)
                                  (thing-get item 'quantity)
                                  (thing-get item 'category)))
           (thing-set! tile 'items (remove item (thing-get tile 'items)))
           (thing-call item 'on-pick-up item entity this)
        
           ; Break out if we successfully stack
           (let/ec break
             (for ([in-inv (in-list (thing-get entity 'inventory))]
                   #:when (eq? (thing-get item 'category)
                               (thing-get in-inv 'category)))
               (thing-set! in-inv 'quantity (+ (thing-get item 'quantity)
                                               (thing-get in-inv 'quantity)))
               (break))
             
             ; If we didn't stack, add it to the inventory
             (thing-set! entity 'inventory (cons item (thing-get entity 'inventory)))))
         
         ; For each item on the ground
         (for ([item (in-list (thing-get tile 'items))])
           ; Remove same typed items from the inventory
           (unless (thing-get item 'stackable)
             (for ([in-inv (in-list (thing-get entity 'inventory))]
                   #:when (eq? (thing-get item 'category)
                               (thing-get in-inv 'category)))
               (drop in-inv)))
           
           ; Pick up or consume the item
           (cond
             [(thing-get item 'stackable)
              (stack item)]
             [(thing-get item 'consumable)
              (consume item)]
             [else
              (pick-up item)]))
         
         ; Look for an on-enter item
         (define on-enter (thing-get tile 'on-enter #f))
         (when on-enter
           (on-enter entity this))]

        ; If it's walkable and occupied, attack the occupant and don't move
        ; damage = max(0, rand(min(1, attack)) - rand(min(1, defense)))
        [else
         (for ([other (in-list others)])
           (send this attack entity other))]))
    
    ; Get a list of all entities by location
    (define/public (get-entities p)
      (for/list ([entity (cons player (get-npcs))]
                 #:when (= p (thing-get entity 'location)))
        entity))
    
    ; Update lighting
    (define/public (update-lighting)
      ; Turn any lit tiles to fog
      (for-tile
       (lambda (x y tile)
         (cond
           [(eq? 'lit (thing-get tile 'lighting 'dark))
            (thing-set! tile 'lighting 'fog)])))
      
      ; Spread lighting from the player
      ; x/y current tile to light
      ; xd/yd direction to spread light:
      ;  if xd != 0, spread to (x+xd y-1) (x+xd y) (x+xd y+1)
      ;  if yd != 0, spread to (x-1 y+yd) (x y+yd) (x-1 y+yd)
      ; spread is initially set to player's light radius
      (define (spread-light x y xd yd spread)
        ; Light the current tile
        (define tile (get-tile x y))
        (thing-set! tile 'lighting 'lit)
        
        ; Recur
        (cond
          ; Don't spread pass solid tiles (spread to them though)
          ; Don't recur past the spread (vision range) value
          [(or (<= spread 0) (thing-get tile 'solid #f))
           (void)]
          ; Spread in the x direction
          [(not (= xd 0))
           (spread-light (+ x xd) (- y 1) xd yd (- spread 1.41))
           (spread-light (+ x xd) y       xd yd (- spread 1.00))
           (spread-light (+ x xd) (+ y 1) xd yd (- spread 1.41))]
          ; Spread in the y direction
          [else
           (spread-light (- x 1) (+ y yd) xd yd (- spread 1.41))
           (spread-light x       (+ y yd) xd yd (- spread 1.00))
           (spread-light (+ x 1) (+ y yd) xd yd (- spread 1.41))]))
      
      ; Spread in the four directions
      (let ([player-x (pt-x (thing-get player 'location))]
            [player-y (pt-y (thing-get player 'location))]
            [player-v (thing-get player 'view-range 5)])
        (spread-light player-x player-y -1  0 player-v)
        (spread-light player-x player-y  1  0 player-v)
        (spread-light player-x player-y  0 -1 player-v)
        (spread-light player-x player-y  0  1 player-v)))
    
    ; Update: NPCs
    (define/public (update)
      (update-npcs this))
    
    ; Draw any npcs (if lit)
    (define/public (draw-npcs canvas)
      (for ([npc (in-list (get-npcs))])
        (define pt (thing-get npc 'location))
        (define draw-pt (recenter canvas (- (thing-get player 'location)
                                            (thing-get npc 'location))))
        
        (define tile (get-tile (pt-x pt) (pt-y pt)))
        
        ; Has to be on screen and lit
        (when (and (<= 0 (pt-x draw-pt) (sub1 (send canvas get-width-in-characters)))
                   (<= 0 (pt-y draw-pt) (sub1 (send canvas get-height-in-characters)))
                   (eq? 'lit (thing-get tile 'lighting 'dark)))
          
          (send canvas write 
                (thing-get npc 'character)
                (pt-x draw-pt) 
                (pt-y draw-pt)
                (thing-get npc 'color)))))
    
    (super-new)))