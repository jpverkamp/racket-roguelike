#lang racket

(provide (all-defined-out))

(require
 "point.rkt"
 "thing/thing.rkt")

; All entities have:
; - a location on the map
; - attack and defense strengths
; - hitpoints
(define-thing entity
  [character #\x]
  [color "white"]
  [location (pt 0 0)])

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
      

; A list of random enemies that we can generate
(define random-enemies
  (vector
   (make-thing fleeing-enemy
     [name "rat"]
     [character #\r])
   (make-thing seeking-enemy
     [name "goblin"]
     [character #\g]
     [color "orange"]
     [attack 15]
     [defense 5])))
    