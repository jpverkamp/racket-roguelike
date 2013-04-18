#lang racket

(provide world%)

(require 
 "entities.rkt"
 "noise/noise.rkt"
 "thing/thing.rkt")

(define world%
  (class object%
    ; Store the player
    (define player (make-thing entity))
    (define/public (get-player) player)
    
    ; Get the contents of a given point, caching for future use
    ; Hash on (x y) => char
    (define tiles (make-hash))
    (define/public (get-tile x y)
      (unless (hash-has-key? tiles (list x y))
        (hash-set! 
         tiles (list x y)
         (let ()
           (define wall?  (> (simplex (* 0.1 x) (* 0.1 y) 0)         0.0))
           (define water? (> (simplex (* 0.1 x) 0         (* 0.1 y)) 0.5))
           (define tree?  (> (simplex 0         (* 0.1 x) (* 0.1 y)) 0.5))
           (cond
             [wall?  'wall]
             [water? 'water]
             [tree?  'tree]
             [else   'empty]))))
      (hash-ref tiles (list x y)))
    
    (super-new)))