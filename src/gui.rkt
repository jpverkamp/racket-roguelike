#lang racket

(provide
 make-gui)

(require
 racket/gui
 racket/draw
 "ascii-canvas/ascii-canvas.rkt"
 "main-menu-screen.rkt")

; Create a new GUI.
(define gui%
  (class object%
    (init-field width-in-chars
                height-in-chars)
    
    ; Create the frame.
    (define frame
      (new frame%
           [label "Racket Roguelike"]
           [style '(no-resize-border)]))
    
    ; Create the ascii canvas
    (define canvas
      (new ascii-canvas%
           [parent frame]
           [width-in-characters width-in-chars]
           [height-in-characters height-in-chars]))
    
    ; The active screen
    (define active-screen (new main-menu-screen%))

    ; Make everything visible
    (send frame show #t)
    
    ; Do the initial drawing
    (send canvas clear)
    (send frame refresh)
    (send active-screen draw canvas)
    (send frame refresh)
    
    ; Finish initilization.
    (super-new)))
           
; Create a new GUI 
(define (make-gui wide high)
  (new gui% 
       [width-in-chars wide]
       [height-in-chars high]))
   
(make-gui 40 24)
    