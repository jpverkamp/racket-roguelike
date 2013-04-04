#lang racket

(provide)

(require
 racket/gui
 racket/draw
 "ascii-canvas/ascii-canvas.rkt")

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
    
    ; Make everything visible
    (send frame show #t)
    
    ; Finish initilization.
    (super-new)))
           
(new gui% 
     [width-in-chars 40]
     [height-in-chars 24])
   
    