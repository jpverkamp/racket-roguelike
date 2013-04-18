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
  [attack 10]
  [defense 10]
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
   (send world try-move 
         me
         (+ (thing-get me 'location)
            (pt (- (random 3) 1)
                (- (random 3) 1))))])

; A list of random enemies that we can generate
(define random-enemies
  (vector
   (make-thing wandering-enemy
    [name "rat"]
    [character #\r])))
    