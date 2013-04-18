#lang racket

(provide (all-defined-out))

; API to use imaginary numbers as points
(define pt make-rectangular)
(define pt-x real-part)
(define pt-y imag-part)

; Convert real coordinates to canvas coordinates
(define (recenter canvas orig)
  (+ orig (pt (quotient (send canvas get-width-in-characters) 2)
              (quotient (send canvas get-height-in-characters) 2))))