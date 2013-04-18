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
  [location (pt 0 0)]
  [attack 1]
  [defense 1]
  [health 10])

; An enemy must have an act method
; It should mutate the world with it's updated state
; The default enemy does nothing
(define-thing enemy entity
  [name "enemy"]
  [color "gray"]
  [(act world)
   (void)])

; A wandering enemy randomly chooses a neighboring open square
(define-thing wandering-enemy enemy
  [(act me world)
   ; Choose a random possible move
   (define possible
     (shuffle
      (for/list ([xd (in-range -1 2)]
                 [yd (in-range -1 2)]
                 #:when (thing-get (send world get-tile 
                                         (+ xd (pt-x (thing-get me 'location)))
                                         (+ yd (pt-y (thing-get me 'location))))
                                   'walkable))
        (+ (thing-get me 'location) (pt xd yd)))))
   
   ; If we found one, move there
   (unless (null? possible)
     (thing-set! me 'location (car possible)))])

; A list of random enemies that we can generate
(define random-enemies
  (vector
   (make-thing wandering-enemy
    [name "rat"]
    [character #\r])))
    