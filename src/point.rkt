#lang racket

(provide (all-defined-out))

; API to use imaginary numbers as points
(define pt make-rectangular)
(define pt-x real-part)
(define pt-y imag-part)
(define pt? complex?)

; Convert real coordinates to canvas coordinates
(define (recenter canvas orig)
  (+ orig (pt (quotient (send canvas get-width-in-characters) 2)
              (quotient (send canvas get-height-in-characters) 2))))

; Calculate the distance between two points
(define (distance p1 p2)
  (define (sqr n) (* n n))
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
  

; Convert a point to a unit point (distance is one for origin)
(define (unit p)
  (define d (distance 0 p))
  (pt (/ (pt-x p) d)
      (/ (pt-y p) d)))