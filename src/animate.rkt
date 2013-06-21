#lang racket

(provide
 animate!
 set-animate!)

(define (animate! . thunks) 
  (void))

(define (set-animate! f)
  (set! animate! f))