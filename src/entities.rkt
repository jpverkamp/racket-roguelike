#lang racket

(provide (all-defined-out))

(require
 thing
 "animate.rkt"
 "point.rkt")

; All entities have:
; - a location on the map
; - attack and defense strengths
; - hitpoints
; - an inventory
(define-thing entity
  [character #\x]
  [color "white"]
  [location (pt 0 0)]
  [inventory '()]
  [view-range 5])

; An enemy must have an act method
; It should mutate the world with it's updated state
; The default enemy does nothing
(define-thing enemy entity
  [name "enemy"]
  [color "gray"]
  [attack 10]
  [defense 10]
  [health 10]
  [(act me world) (void)])

; A wandering enemy randomly chooses a neighboring open square
(define-thing wandering-enemy enemy
  [(act me world)
   ; Choose a random possible move
   (send world try-move 
         me
         (+ (thing-get me 'location)
            (pt (- (random 3) 1)
                (- (random 3) 1))))])

; A seeking enemy runs towards the player (heedless of walls) 50% of the time
; The other 50% of the time they are identical to a wandering enemy
(define-thing seeking-enemy wandering-enemy
  [(act me world)
   (cond
     ; 50/50 of a seeking move
     [(= 0 (random 2))
      (define player-pt (thing-get (send world get-player) 'location))
      (define me-pt (thing-get me 'location))
      (define dir (unit (- player-pt me-pt)))
      (send world try-move
            me
            (+ me-pt
               (inexact->exact (round (pt-x dir)))
               (inexact->exact (round (pt-y dir)))))]
     ; Otherwise, wander
     [else
      (thing-call wandering-enemy 'act me world)])])

; A fleeing enemy moves the exact opposite of a seeking enemy
(define-thing fleeing-enemy wandering-enemy
  [(act me world)
   (cond
     ; 50/50 of a fleeing move
     [(= 0 (random 2))
      (define player-pt (thing-get (send world get-player) 'location))
      (define me-pt (thing-get me 'location))
      (define dir (unit (- player-pt me-pt)))
      (send world try-move
            me
            (- me-pt
               (inexact->exact (round (pt-x dir)))
               (inexact->exact (round (pt-y dir)))))]
     ; Otherwise, wander
     [else
      (thing-call wandering-enemy 'act me world)])])
      
; An exploding enemy blows up whenever the player gets close to them
; Otherwise, they cannot move or attack (by default)
(define-thing exploding-enemy enemy
  [(act me world)
   (define distance-to-player
     (distance (thing-get (send world get-player) 'location)
               (thing-get me 'location)))
   (when (<= distance-to-player 1.5)
     ; Log message
     (send world log (format "~a explodes violently" (thing-get me 'name)))

     ; Get the neighboring tiles
     (define neighbors 
       (for*/list ([xd (in-range -1 2)]
                   [yd (in-range -1 2)])
         (define loc (+ (thing-get me 'location) (pt xd yd)))
         (send world tile-at (pt-x loc) (pt-y loc))))
     
     ; Store the original color
     (for ([tile (in-list neighbors)])
       (thing-set! tile 'original-color (thing-get tile 'color))
       (thing-set! tile 'original-character (thing-get tile 'character))
       (thing-set! tile 'character #\*))
     
     ; Animate through several colors
     (define (random-color) (vector-ref (vector "red" "yellow" "white") (random 3)))
     (for ([i (in-range 3)])
       (animate!
        (lambda ()
          (for ([tile (in-list neighbors)])
            (thing-set! tile 'color (random-color))
            (sleep 0.01)))))
     
     ; Clear them 
     (animate!
      (lambda ()
        (for ([tile (in-list neighbors)])
          (thing-set! tile 'color (thing-get tile 'original-color))
          (thing-set! tile 'character (thing-get tile 'original-character)))))
     
     ; Damage neighbors
     (for* ([xd (in-range -1 2)]
            [yd (in-range -1 2)])
       (define loc (+ (thing-get me 'location) (pt xd yd)))
       (for ([other (send world get-entities loc)])
         (unless (eqv? me other)
           (send world attack me other))))
     
     ; Destroy self
     (thing-set! me 'health -1))])
   
; Actual enemy definitions
(define *entities*
  (vector
   (make-thing exploding-enemy
     [name "bomb"]
     [color "white"]
     [character #\O]
     [attack 50])

   (make-thing fleeing-enemy
     [name "rat"]
     [character #\r]
     [color "brown"])
   
   (make-thing seeking-enemy
     [name "minotaur"]
     [character #\M]
     [color "brown"])
   
   (make-thing fleeing-enemy
     [name "spider"]
     [character #\s]
     [color "silver"]
     [(act me world)
      (define loc (thing-get me 'location))
      (define tile (send world tile-at (pt-x loc) (pt-y loc)))
      
      ; Try to move
      (thing-call seeking-enemy 'act me world)
      
      ; Check if we did (the old location is now empty)      
      ; If so, make that tile a web that fades when walked on
      (when (null? (send world get-entities loc))
        (define old-tile (make-thing tile))
        
        (thing-set! tile 'character (integer->char 206))
        (thing-set! tile 'color "silver")
        (thing-set! tile 'solid #t)
        
        (thing-set! tile 'on-enter
          (lambda (entity world)
            (for ([key (in-list '(character color lighting walkable solid))])
              (thing-set! tile key (thing-get old-tile key))
              (thing-set! tile 'on-enter (lambda _ (void)))))))])
   
   (make-thing seeking-enemy
     [name "goblin"]
     [character #\g]
     [color "orange"]
     [attack 15]
     [defense 5])

   (make-thing seeking-enemy
     [name "bomber"]
     [character #\b]
     [color "orange"]
     [attack 15]
     [defense 5]
     [(act me world)
      (thing-call seeking-enemy 'act me world)
      (thing-call exploding-enemy 'act me world)])))