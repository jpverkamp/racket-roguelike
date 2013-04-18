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
  [location (pt 0 0)]
  [attack 1]
  [defense 1]
  [health 10])
