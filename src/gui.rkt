#lang racket

(provide
 make-gui)

(require
 racket/gui
 racket/draw
 ascii-canvas
 "screen.rkt"
 "main-menu-screen.rkt"
 "animate.rkt")

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
      (new (class ascii-canvas%
             (inherit-field 
              width-in-characters
              height-in-characters)
             
             ; Process keyboard events
             (define/override (on-char key-event)
               (case (send key-event get-key-code)
                 ; Exit the program
                 [(escape) (exit)]
                 ; Ignore key release events and pressing alt
                 [(release menu) (void)]
                 ; Pass everything along to the screen
                 ; Update the screen to whatever it returns
                 [else
                  (set! active-screen (send active-screen update key-event))
                  (cond
                    ; If it's still a valid screen, redraw it
                    [(is-a? active-screen screen%)
                     (animate!)]
                    ; Otherwise, exit the program
                    [else
                     (exit)])]))
             
             ; Initialize the ascii-canvas fields
             (super-new
              [parent frame]
              [width-in-characters width-in-chars]
              [height-in-characters height-in-chars]))))
    
    ; Correctly set the animation function
    (set-animate!
      (lambda thunks 
        (for-each (lambda (thunk) (thunk)) thunks)
        (send active-screen draw canvas)
        (send frame refresh)
        (yield)))
    
    ; The active screen
    (define active-screen (new main-menu-screen%))

    ; Make everything visible
    (send frame show #t)
    
    ; Do the initial drawing
    (animate!)
    
    ; Finish initilization.
    (super-new)))
           
; Create a new GUI 
(define (make-gui wide high)
  (new gui% 
       [width-in-chars wide]
       [height-in-chars high]))